module internal XRoad.Emitter

open Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.EmitterDsl
open XRoad.Serialization.Attributes
open XRoad.XmlExtensions

type Serialization =
    { Root: MethodInfo
      Content: MethodInfo }
    with
        static member Create (typ: Type): Serialization =
            { Root = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true)
              Content = DynamicMethod(sprintf "%s_SerializeContent" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true) }

type Deserialization =
    { Root: MethodInfo
      Content: MethodInfo
      MatchType: MethodInfo }
    with
        static member Create (typ: Type): Deserialization =
            { Root = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |], true)
              Content = DynamicMethod(sprintf "%s_DeserializeContent" typ.FullName, typeof<bool>, [| typeof<XmlReader>; typeof<obj>; typeof<bool>; typeof<SerializerContext> |], true)
              MatchType = DynamicMethod(sprintf "%s_MatchType" typ.FullName, typeof<bool>, [| typeof<XmlReader> |], true) }

type TypeMap =
    { Type: Type
      Name: string
      Namespace: string option
      Layout: LayoutKind option
      IsAnonymous: bool
      DeserializeDelegate: Lazy<DeserializerDelegate>
      Deserialization: Deserialization
      SerializeDelegate: Lazy<SerializerDelegate>
      Serialization: Serialization
      CanHaveNullAsValue: bool
      BaseType: TypeMap option
      mutable IsComplete: bool }
    member this.Serialize(writer: XmlWriter, value: obj, context: SerializerContext) =
        this.SerializeDelegate.Value.Invoke(writer, value, context)
    member this.Deserialize(reader: XmlReader, context: SerializerContext) =
        this.DeserializeDelegate.Value.Invoke(reader, context)
    member this.FullName =
        match this.Namespace with
        | Some(ns) -> sprintf "%s:%s" ns this.Name
        | None -> this.Name
    static member Create(typ: Type, deserialization, serialization, baseType) =
        let attr = typ.GetCustomAttribute<XRoadTypeAttribute>() |> Option.ofObj
        let layout = attr |> Option.map (fun attr -> attr.Layout)
        { Type = typ
          Name = attr |> Option.fold (fun name attr -> match attr.Name with null | "" -> name | x -> x) typ.Name
          Namespace = attr |> Option.bind (fun attr -> match attr.Namespace with null | "" -> None | x -> Some(x))
          Layout = layout
          IsAnonymous = attr |> Option.map (fun attr -> attr.IsAnonymous) |> MyOption.defaultValue false
          Deserialization = deserialization
          DeserializeDelegate = lazy (deserialization.Root.CreateDelegate(typeof<DeserializerDelegate>) |> unbox)
          Serialization = serialization
          SerializeDelegate = lazy (serialization.Root.CreateDelegate(typeof<SerializerDelegate>) |> unbox)
          CanHaveNullAsValue = (not (Nullable.GetUnderlyingType(typ) |> isNull)) || (typ.IsClass && layout <> Some(LayoutKind.Choice))
          BaseType = baseType
          IsComplete = false }

type ContentWrapper =
    | Choice of TypeMap * FieldInfo * FieldInfo * int * MethodInfo
    | Method of MethodInfo * int32
    | Type of TypeMap
    member this.Name
        with get () =
            match this with Choice (tm,_,_,_,_) -> tm.Type.FullName | Method (mi, _) -> sprintf "%s.%s" mi.DeclaringType.FullName mi.Name | Type tm -> tm.FullName

type PropertyMap =
    { TypeMap: TypeMap
      SimpleTypeName: XmlQualifiedName option
      Element: (XName * bool * bool) option
      Wrapper: ContentWrapper
      GetMethod: MethodInfo option
      SetMethod: MethodInfo option
      HasValueMethod: MethodInfo option }
    member this.HasOptionalElement with get() = match this.Element with Some(_, _, true) -> true | _ -> false

type ArrayMap =
    { Type: Type
      Element: (XName * bool * bool) option
      ItemTypeMap: TypeMap
      ItemElement: (XName * bool * bool) option
      ItemSimpleTypeName: XmlQualifiedName option
      Wrapper: ContentWrapper
      GetMethod: MethodInfo option
      SetMethod: MethodInfo option
      HasValueMethod: MethodInfo option }
    member this.GetItemPropertyMap() =
        { TypeMap = this.ItemTypeMap
          SimpleTypeName = this.ItemSimpleTypeName
          Element = this.ItemElement
          Wrapper = this.Wrapper
          GetMethod = None
          SetMethod = None
          HasValueMethod = None }
    member this.HasOptionalElement with get() = match this.Element with Some(_, _, true) -> true | _ -> false

type Property =
    | Individual of PropertyMap
    | Array of ArrayMap
    member this.Element with get() = this |> function Individual x -> x.Element | Array x -> x.Element
    member this.Wrapper with get() = this |> function Individual x -> x.Wrapper | Array x -> x.Wrapper
    member this.Type with get() = this |> function Individual x -> x.TypeMap.Type | Array x -> x.Type
    member this.GetMethod with get() = this |> function Individual x -> x.GetMethod | Array x -> x.GetMethod
    member this.SetMethod with get() = this |> function Individual x -> x.SetMethod | Array x -> x.SetMethod
    member this.PropertyName
        with get() =
            match this with
            | Individual { Element = Some(name,_,_) }
            | Array { Element = Some(name,_,_) }
            | Array { Element = None; ItemElement = Some(name,_,_) } -> Some(name)
            | _ -> None
    member this.SimpleTypeName
        with get() =
            match this with
            | Individual(x) -> x.SimpleTypeName
            | Array(_) -> Some(XmlQualifiedName("Array", XmlNamespace.SoapEnc))
    member this.HasValueMethod with get() = match this with | Individual(x) -> x.HasValueMethod | Array(x) -> x.HasValueMethod
    member this.HasOptionalElement with get() = match this with Individual(x) -> x.HasOptionalElement | Array(x) -> x.HasOptionalElement

let firstRequired (properties: Property list) =
    properties
    |> List.tryPick (fun p -> match p.Element with Some(_,_,false) -> Some(p) | _ -> None)

type private XopBinaryContent() =
    inherit BinaryContent("", Data [| |])

let (|Serializable|NotSerializable|) (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> NotSerializable
    | attr -> Serializable(attr)

let safe (name: XName) = if name.NamespaceName = "" then name.LocalName else sprintf "%s:%s" name.NamespaceName name.LocalName

type PropertyInput = ContentWrapper * string * Type * bool * MethodInfo option * MethodInfo option * MethodInfo option * XRoadElementAttribute * XRoadCollectionAttribute option

let getContentOfType (typeMap: TypeMap) : PropertyInput list =
    typeMap.Type.GetProperties(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
    |> List.ofArray
    |> List.sortBy (fun p -> p.MetadataToken)
    |> List.choose (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj |> Option.map (fun x -> (p, x)))
    |> List.map
        (fun (p, attr) ->
            let propertyType, isOptionalType, hasValueMethod =
                if p.PropertyType.IsGenericType && p.PropertyType.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                    p.PropertyType.GenericTypeArguments.[0], true, Some(p.PropertyType.GetProperty("HasValue").GetGetMethod())
                else p.PropertyType, false, None
            Type typeMap, p.Name, propertyType, isOptionalType, hasValueMethod, Some(p.GetGetMethod()), Some(p.GetSetMethod(true)), attr, p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj)

let getContentOfMethod (mi: MethodInfo) : PropertyInput list =
    mi.GetParameters()
    |> Seq.choose (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj |> Option.map (fun x -> (p, x)))
    |> Seq.mapi (fun i (p, attr) ->
        let parameterType, isOptionalType, hasValueMethod =
            if p.ParameterType.IsGenericType && p.ParameterType.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                p.ParameterType.GenericTypeArguments.[0], true, Some(p.ParameterType.GetProperty("HasValue").GetGetMethod())
            else p.ParameterType, false, None
        Method(mi, i), p.Name, parameterType, isOptionalType, hasValueMethod, None, None, attr, p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj)
    |> Seq.toList

let getContentOfChoice (choiceMap: TypeMap) : PropertyInput list =
    let choiceType = choiceMap.Type
    let idField =
        match choiceType.GetField("__id", BindingFlags.Instance ||| BindingFlags.NonPublic) with
        | null -> choiceType.GetField("__id@", BindingFlags.Instance ||| BindingFlags.NonPublic)
        | x -> x
    let valueField =
        match choiceType.GetField("__value", BindingFlags.Instance ||| BindingFlags.NonPublic) with
        | null -> choiceType.GetField("__value@", BindingFlags.Instance ||| BindingFlags.NonPublic)
        | x -> x
    choiceType.GetCustomAttributes<XRoadElementAttribute>()
    |> Seq.map
        (fun attr ->
            let (typ, mi) =
                let methodName = sprintf "New%s%s" (if Char.IsLower(attr.Name.[0]) then "_" else "") attr.Name
                match choiceType.GetMethod(methodName, BindingFlags.Public ||| BindingFlags.Static) with
                | null -> failwithf "Type `%s` should define public static method `%s`." choiceType.FullName methodName
                | mi -> match mi.GetParameters() with
                        | [| pi |] -> (pi.ParameterType, mi)
                        | _ -> failwithf "Type `%s` method `New%s` should have exactly one argument." choiceType.FullName attr.Name
            let parameterType, isOptionalType, hasValueMethod =
                if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                    typ.GenericTypeArguments.[0], true, Some(typ.GetProperty("HasValue").GetGetMethod())
                else typ, false, None
            let collectionAttr = choiceType.GetCustomAttributes<XRoadCollectionAttribute>() |> Seq.tryFind (fun a -> a.Id = attr.Id)
            Choice (choiceMap, idField, valueField, attr.Id, mi), attr.Name, parameterType, isOptionalType, hasValueMethod, None, None, attr, collectionAttr)
    |> Seq.toList

module EmitSerialization =
    /// Check if values type matches expected type.
    let private emitValueTypeTest (expectedType: Type) =
        loadArg1
        >> callX <@ (null: obj).GetType() @>
        >> callVirtX <@ (null: Type).FullName @>
        >> loadString expectedType.FullName
        >> stringEquals

    /// Write type attribute according to TypeMap.
    let emitTypeAttribute (typeName: string) (typeNamespace: string option) =
        loadArg0
        >> loadString "type"
        >> loadString XmlNamespace.Xsi
        >> callVirtX <@ (null: XmlWriter).WriteStartAttribute("", "") @>
        >> noop
        >> loadArg0
        >> loadString typeName
        >> ifSomeNone
                typeNamespace
                (fun ns ->
                    loadString ns
                    >> callVirtX <@ (null: XmlWriter).WriteQualifiedName("", "") @>)
                (callVirtX <@ (null: XmlWriter).WriteString("") @>)
        >> noop
        >> loadArg0
        >> callVirtX <@ (null: XmlWriter).WriteEndAttribute() @>
        >> noop

    /// Emit type (and its base types) content serialization.
    let rec private emitContentSerialization (typeMap: TypeMap) =
        ifSome typeMap.BaseType emitContentSerialization
        >> loadArg0
        >> loadArg1
        >> loadArg2
        >> call typeMap.Serialization.Content
        >> noop

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (typeMap: TypeMap) =
        loadString (sprintf "Cannot serialize abstract type `%s`." typeMap.FullName)
        >> createX <@ Exception("") @>
        >> throw

    /// Emit whole contents of TypeMap serialization.
    let private emitBodySerialization addType (typeMap: TypeMap) =
        ifElse typeMap.Type.IsAbstract
            (emitAbstractTypeException typeMap)
            (iif addType (emitTypeAttribute typeMap.Name typeMap.Namespace)
             // TODO : Attributes
             >> emitContentSerialization typeMap)

    /// Emit serialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchySerialization (markReturn: Label) isEncoded (subTypes: TypeMap list) typeMap =
        match subTypes with
        | [] ->
            emitBodySerialization isEncoded typeMap
        | subType::other ->
            beforeLabel (fun markNext ->
                // Check if type matches current TypeMap.
                emitValueTypeTest subType.Type
                >> gotoF markNext
                >> emitBodySerialization true subType
                >> goto markReturn)
            >> noop
            >> emitTypeHierarchySerialization markReturn isEncoded other typeMap

    let emitNilAttribute (markReturn: Label) =
        beforeLabel (fun markNotNull ->
            loadNull
            >> equals
            >> gotoF markNotNull
            >> loadArg0
            >> loadString "nil"
            >> loadString XmlNamespace.Xsi
            >> loadString "true"
            >> callVirtX <@ (null: XmlWriter).WriteAttributeString("", "", "") @>
            >> noop
            >> goto markReturn)
        >> noop

    /// Emit root type serialization logic for given TypeMap.
    let emitRootSerializerMethod isEncoded (subTypes: TypeMap list) (typeMap: TypeMap) =
        beforeLabel (fun markReturn ->
            // When value is `null`, write `xsi:nil` attribute and return.
            loadArg1
            >> emitNilAttribute markReturn
            // Serialize value according to its type.
            >> emitTypeHierarchySerialization markReturn isEncoded subTypes typeMap)
        // Return
        >> ret

    /// Provides value for array item at current index.
    let private emitArrayItemValue (array: LocalBuilder) (index: LocalBuilder) (typ: Type) =
        getVar array
        >> getVar index
        >> getElem typ
        >> iif typ.IsValueType (toBox typ)

    /// Emit validation for not nullable types to have value specified.
    let private emitNotNullableCheck (name: string) emitValue property =
        match property with
        | Array _
        | Individual { TypeMap = { CanHaveNullAsValue = true } } ->
            beforeLabel (fun markSuccess ->
                // Check if value is null.
                emitValue property.Type
                >> loadNull
                >> equals
                >> gotoF markSuccess
                // Not nullable shouldn't have null as value, so throw exception.
                >> loadString (sprintf "Not nullable property `%s` of type `%s` has null value." name property.Wrapper.Name)
                >> createX <@ Exception("") @>
                >> throw)
            >> noop
        | _ -> id

    let emitPropertyWrapperSerialization (property: Property) =
        match property.Wrapper with
        | Choice (_,_,fld,_,_) ->
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> MyOption.defaultWith (fun _ -> property.Type)
            loadArg1
            >> castClass fld.DeclaringType
            >> getField fld
            >> ifElse property.Type.IsValueType (fromBox ty) (castClass ty)
        | Method (mi, i) ->
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> MyOption.defaultWith (fun _ -> property.Type)
            loadArg1
            >> loadInt i
            >> getElemRef
            >> ifElse ty.IsValueType (fromBox ty) (castClass ty)
        | Type tm ->
            loadArg1
            >> castClass tm.Type
            >> callVirt property.GetMethod.Value

    let emitOptionalFieldSerialization (property: Property) emitContent =
        if not property.HasOptionalElement then emitContent else
        emitPropertyWrapperSerialization property
        >> useVar (lazy (declareLocal property.HasValueMethod.Value.DeclaringType)) (fun optionalType ->
            setVar optionalType
            >> getVarAddr optionalType
            >> call property.HasValueMethod.Value
            >> beforeLabel (fun endContentLabel ->
                gotoF endContentLabel
                >> noop
                >> emitContent)
            >> noop)

    /// Emit single property content serialization.
    let rec emitPropertyContentSerialization emitValue isEncoded (property: Property) : ILGenerator -> ILGenerator =
        // Write start element of the propery if its not merged with content.
        let writeStartElement =
            ifSome property.Element (fun (name, isNullable, _) ->
                loadArg0
                >> loadString name.LocalName
                >> loadString name.NamespaceName
                >> callVirtX <@ (null: XmlWriter).WriteStartElement("", "") @>
                >> noop
                >> iif (not isNullable) (emitNotNullableCheck name.LocalName emitValue property)
                >> iif (isEncoded) (
                    ifSome property.SimpleTypeName (fun typeName -> emitTypeAttribute typeName.Name (Some(typeName.Namespace)))))

        // Serialize property content value according to its TypeMap.
        let writePropertyContent =
            beforeLabel (fun markReturn ->
                match property with
                | Individual propertyMap ->
                    loadArg0
                    >> emitValue propertyMap.TypeMap.Type
                    >> loadArg2
                    >> call propertyMap.TypeMap.Serialization.Root
                    >> noop
                | Array arrayMap ->
                    emitValue arrayMap.Type
                    >> emitNilAttribute markReturn
                    >> emitValue arrayMap.Type
                    >> useVar (lazy (declareLocal arrayMap.Type)) (fun arr ->
                        setVar arr
                        >> useVar (lazy (declareLocalOf<int>)) (fun i ->
                            loadInt0
                            >> setVar i
                            >> withLabel (fun markLoopCondition ->
                                goto markLoopCondition
                                >> afterLabel (fun markLoopStart ->
                                    let itemPropertyMap = Individual (arrayMap.GetItemPropertyMap())
                                    noop
                                    >> (emitPropertyContentSerialization (emitArrayItemValue arr i) isEncoded itemPropertyMap)
                                    >> getVar i
                                    >> loadInt1
                                    >> add
                                    >> setVar i
                                    >> setLabel markLoopCondition
                                    >> getVar i
                                    >> getVar arr
                                    >> getLen
                                    >> castInt
                                    >> lessThan
                                    >> gotoT markLoopStart)))))
            >> noop

        // Write end element if required.
        let writeEndElement =
            ifSome property.Element (fun _ ->
                loadArg0
                >> callVirtX <@ (null: XmlWriter).WriteEndElement() @>
                >> noop)

        writeStartElement >> writePropertyContent >> writeEndElement

    /// Unbox property value into correct type.
    let emitPropertyValue (property: Property) (typ: Type) =
        emitPropertyWrapperSerialization property
        >> iif property.HasOptionalElement (
            useVar (lazy (declareLocal property.HasValueMethod.Value.DeclaringType)) (fun opt ->
                setVar opt
                >> getVarAddr opt
                >> ifElse typ.IsValueType
                        (useVar (lazy (declareLocal typ)) (fun temp ->
                            getVarAddr temp
                            >> initObj typ
                            >> getVar temp))
                        loadNull
                >> call (property.HasValueMethod.Value.DeclaringType.GetMethod("ValueOr", [| typ |]))))
        >> iif typ.IsValueType (toBox typ)

    /// Emit IL which serializes each property value into corresponding xml fragment.
    let emitContentSerializerMethod isEncoded (properties: Property list) =
        properties
        |> List.map (fun property -> emitOptionalFieldSerialization property (emitPropertyContentSerialization (emitPropertyValue property) isEncoded property))
        |> (fun items -> if items.IsEmpty then id else items |> List.reduce (fun a b -> a >> b))

module EmitDeserialization =
    /// Check if current element has `xsi:nil` attribute present.
    let private emitNullCheck = emit' {
        ldarg_0
        call_expr <@ (null: XmlReader).ReadXsiNullAttribute() @>
    }

    /// Reads type attribute value and stores name and namespace in variables.
    let private emitTypeAttributeRead = emit' {
        ldarg_0
        call_expr <@ (null: XmlReader).ReadXsiTypeAttribute() @>
    }

    /// Check if value type matches expected type.
    let private emitValueTypeTest isDefault (qualifiedNameVar: LocalBuilder) (typeMap: TypeMap) = emit' {
        ldarg_0
        ldloc qualifiedNameVar
        ldstr typeMap.Name
        ldstr (typeMap.Namespace |> MyOption.defaultValue "")
        if_else typeMap.IsAnonymous (emit' { ldc_i4_1 }) (emit' { ldc_i4_0 })
        if_else isDefault (emit' { ldc_i4_1 }) (emit' { ldc_i4_0 })
        call_expr <@ (null: XmlReader).IsQualifiedTypeName(null, "", "", false, false) @>
    }

    /// Emit type (and its base types) content deserialization.
    let rec private emitContentDeserialization (instance: LocalBuilder) (typeMap: TypeMap) =
        loadArg0
        >> getVar instance
        >> loadInt0
        >> loadArg1
        >> call typeMap.Deserialization.Content

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (typeMap: TypeMap) =
        loadString (sprintf "Cannot deserialize abstract type `%s`." typeMap.FullName)
        >> createX <@ Exception("") @>
        >> throw

    let private emitXmlReaderRead =
        loadArg0
        >> callVirtX <@ (null: XmlReader).Read() @>
        >> noop

    let private emitXmlReaderReadOrExcept (propertyName: XName option) =
        let errorMessage =
            match propertyName with
            | Some(name) -> sprintf "Invalid message: expected `%s`, but was end of file." (safe name)
            | None -> "Invalid message: unexpected end of file."
        emitXmlReaderRead
        >> beforeLabel (fun markSuccess ->
            gotoT markSuccess
            >> loadString errorMessage
            >> createX <@ Exception("") @>
            >> throw)
        >> noop

    let private emitMoveToEndOrNextElement (doneLabel: Label) (skipVar: LocalBuilder, depthVar: LocalBuilder) name =
        getVar skipVar
        >> beforeLabel (fun skipReadLabel -> gotoT skipReadLabel >> emitXmlReaderReadOrExcept name)
        >> loadInt0
        >> setVar skipVar
        >> loadArg0
        >> callVirtX <@ (null: XmlReader).NodeType @>
        >> loadInt XmlNodeType.EndElement
        >> equals
        >> gotoF doneLabel
        >> loadArg0
        >> callVirtX <@ (null: XmlReader).Depth @>
        >> getVar depthVar
        >> lessThan
        >> gotoF doneLabel

    let emitWrongElementException expectedValue wrapper =
        let wrapperName =
            match wrapper with
            | Choice _ -> sprintf "choice `%s`" wrapper.Name
            | Method _ -> sprintf "operation `%s` wrapper element" wrapper.Name
            | Type _ -> sprintf "type `%s`" wrapper.Name
        loadString (sprintf "Element `%s` was expected in subsequence of %s, but element `{0}` was found instead." expectedValue wrapperName)
        >> loadArg0
        >> callVirtX <@ (null: XmlReader).LocalName @>
        >> stringFormat2
        >> createX <@ Exception("") @>
        >> throw

    /// Emit whole contents of TypeMap deserialization.
    let private emitBodyDeserialization (returnLabel: Label) (skipVar: LocalBuilder) (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then emitAbstractTypeException typeMap else
        create (typeMap.Type.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public, null, [| |], [| |]))
        >> useVar (lazy (declareLocal typeMap.Type)) (fun instance ->
            setVar instance
            // TODO : Attributes
            >> emitContentDeserialization instance typeMap
            >> setVar skipVar
            >> getVar instance)

    /// Emit deserialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchyDeserialization (markReturn: Label) (skipVar: LocalBuilder) (subTypes: TypeMap list) qualifiedName typeMap =
        match subTypes with
        | [] ->
            let context = if typeMap.IsAnonymous then "anonymous type" else sprintf "type `%s`" (XName.Get(typeMap.Name, typeMap.Namespace |> MyOption.defaultValue "") |> safe)
            beforeLabel (fun errorLabel ->
                emitValueTypeTest true qualifiedName typeMap
                >> gotoF errorLabel
                >> emitBodyDeserialization markReturn skipVar typeMap
                >> goto markReturn)
            >> loadString (sprintf "Unexpected type value: using type `{0}` is not allowed in the context of %s." context)
            >> getVar qualifiedName
            >> stringFormat2
            >> createX <@ Exception("") @>
            >> throw
        | subType::other ->
            beforeLabel (fun markNext ->
                // Check if type matches current TypeMap.
                emitValueTypeTest false qualifiedName subType
                >> gotoF markNext
                // Deserialize content
                >> emitBodyDeserialization markReturn skipVar subType
                >> goto markReturn)
            >> noop
            >> emitTypeHierarchyDeserialization markReturn skipVar other qualifiedName typeMap

    let emitRootDeserializerMethod hasInlineContent (subTypes: TypeMap list) (typeMap: TypeMap) =
        useVar (lazy declareLocalOf<int>) (fun depthVar ->
            loadArg0
            >> callVirtX <@ (null: XmlReader).Depth @>
            >> loadInt1
            >> add
            >> setVar depthVar
            >> useVar (lazy declareLocalOf<bool>) (fun skipVar ->
                // When value nil attribute is present returns null.
                beforeLabel (fun markReturn ->
                        emitNullCheck
                        >> beforeLabel (fun label ->
                            gotoF label
                            >> loadNull
                            >> goto markReturn)
                        // Read type attribute value of current element.
                        >> useVar (lazy declareLocalOf<XmlQualifiedName>) (fun qualifiedName ->
                            emitTypeAttributeRead
                            >> setVar qualifiedName
                            // Serialize value according to its type.
                            >> emitTypeHierarchyDeserialization markReturn skipVar subTypes qualifiedName typeMap))
                >> iif (not hasInlineContent)
                        (afterLabel (fun startLabel ->
                            beforeLabel (fun successLabel ->
                                beforeLabel (fun doneLabel ->
                                    emitMoveToEndOrNextElement doneLabel (skipVar, depthVar) None
                                    >> goto successLabel)
                                // reader.Depth != depth
                                >> loadArg0
                                >> callVirtX <@ (null: XmlReader).Depth @>
                                >> getVar depthVar
                                >> equals
                                >> gotoF startLabel
                                // reader.NodeType != XmlNodeType.Element
                                >> loadArg0
                                >> callVirtX <@ (null: XmlReader).NodeType @>
                                >> loadInt XmlNodeType.Element
                                >> equals
                                >> gotoF startLabel
                                >> emitWrongElementException "<end of element>" (Type typeMap))))))
        >> ret

    let emitPropertyValueDeserialization (isContent: bool) (typeMap: TypeMap) = emit' {
        ldarg_0
        if_else isContent (emit' {
            ldarg_3
        }) (emit' {
            ldarg_1
        })
        call typeMap.Deserialization.Root
        if_else typeMap.Type.IsValueType (emit' {
            unbox typeMap.Type
        }) (emit' {
            castclass typeMap.Type
        })
    }

    let emitPropertyWrapperDeserialization (wrapper: ContentWrapper) =
        match wrapper with
        | Choice _ -> id
        | Method _ -> failwith "Method signature is not deserialzable"
        | Type tm -> emit' {
            ldarg_1
            castclass tm.Type
        }

    let optionalSomeMethod =
        typeof<Optional.Option>.GetMethods()
        |> Array.filter (fun m -> m.Name = "Some" && m.GetGenericArguments().Length = 1)
        |> Array.exactlyOne

    let emitIndividualPropertyDeserialization isContent (propertyMap: PropertyMap) = emit' {
        merge (emitPropertyValueDeserialization isContent propertyMap.TypeMap)
        declare_variable (lazy (declareLocal propertyMap.TypeMap.Type)) (fun x -> emit' {
            stloc x
            merge (emitPropertyWrapperDeserialization propertyMap.Wrapper)
            ldloc x
            iif propertyMap.HasOptionalElement (emit' {
                call (optionalSomeMethod.MakeGenericMethod([| propertyMap.TypeMap.Type |]))
            })
        })
    }

    let emitArrayContentEndCheck (markArrayEnd: Label) (skipVar: LocalBuilder, depthVar: LocalBuilder) = emit' {
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NodeType @>
        ldc_node_type XmlNodeType.EndElement
        ceq
        define_label (fun markSuccess -> emit' {
            brfalse markSuccess
            ldarg_0
            callvirt_expr <@ (null: XmlReader).Depth @>
            ldloc depthVar
            clt
            brfalse markSuccess
            ldc_i4_1
            stloc skipVar
            br markArrayEnd
            set_marker markSuccess
        })
        nop
    }

    let emitXmlReaderDepthCheck (varDepth: LocalBuilder) = emit' {
        ldarg_0
        callvirt_expr <@ (null: XmlReader).Depth @>
        ldloc varDepth
        ceq
    }

    let emitXmlReaderNodeTypeCheck = emit' {
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NodeType @>
        ldc_i4_1
        ceq
    }

    let emitArrayItemDeserialization isContent (arrayMap: ArrayMap, listInstance: LocalBuilder, markEnd: Label, stopIfWrongElement) =
        ifSome arrayMap.ItemElement (fun (name, _, _) ->
            loadArg0
            >> callVirtX <@ (null: XmlReader).LocalName @>
            >> loadString name.LocalName
            >> stringEquals
            >> beforeLabel (fun markDeserialize ->
                    beforeLabel (fun markError ->
                        gotoF markError
                        >> loadArg0
                        >> callVirtX <@ (null: XmlReader).NamespaceURI @>
                        >> loadString name.NamespaceName
                        >> stringEquals
                        >> gotoT markDeserialize)
                    >> ifElse stopIfWrongElement
                            (goto markEnd)
                            (loadString "Unexpected element: found `{0}`, but was expecting to find `{1}`."
                             >> loadArg0
                             >> callVirtX <@ (null: XmlReader).LocalName @>
                             >> loadString (safe name)
                             >> stringFormat3
                             >> createX <@ Exception("") @>
                             >> throw)))
        >> getVar listInstance
        >> emitPropertyValueDeserialization isContent arrayMap.ItemTypeMap
        >> callVirt (listInstance.LocalType.GetMethod("Add", [| arrayMap.ItemTypeMap.Type |]))

    /// Emits array type deserialization logic.
    let emitArrayPropertyDeserialization isContent skipVar (arrayMap: ArrayMap) =
        let listType = typedefof<System.Collections.Generic.List<_>>.MakeGenericType(arrayMap.ItemTypeMap.Type)
        emit' {
            declare_variable (lazy declareLocalOf<int>) (fun depthVar -> emit' {
                ldarg_0
                callvirt_expr <@ (null: XmlReader).Depth @>
                stloc depthVar
                define_labels 4 (fun (List4 (markArrayNull, markSkipRead, markArrayEnd, markLoopStart)) -> emit' {
                    iif arrayMap.Element.IsSome (emit' {
                        merge emitNullCheck
                        define_label (fun label -> emit' {
                            brfalse label
                            ldnull
                            br markArrayNull
                            set_marker label
                        })
                        ldloc depthVar
                        ldc_i4_1
                        add
                        stloc depthVar
                    })
                    declare_variable (lazy (declareLocal listType)) (fun listInstance -> emit'{
                        newobj (listType.GetConstructor([||]))
                        stloc listInstance
                        if_else arrayMap.Element.IsSome
                            (emit' {
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).IsEmptyElement @>
                                brtrue markArrayEnd
                            })
                            (emit' {
                                br markSkipRead
                            })
                        set_marker markLoopStart
                        merge (emitXmlReaderReadOrExcept (arrayMap.ItemElement |> Option.map (fun (x,_,_) -> x)))
                        set_marker markSkipRead
                        merge (emitArrayContentEndCheck markArrayEnd (skipVar, depthVar))
                        merge (emitXmlReaderDepthCheck depthVar)
                        brfalse markLoopStart
                        merge emitXmlReaderNodeTypeCheck
                        brfalse markLoopStart
                        merge (emitArrayItemDeserialization isContent (arrayMap, listInstance, markArrayEnd, arrayMap.Element.IsNone))
                        br markLoopStart
                        set_marker markArrayEnd
                        declare_variable (lazy (declareLocal arrayMap.Type)) (fun instance -> emit' {
                            ldloc listInstance
                            callvirt (listType.GetMethod("ToArray", [| |]))
                            set_marker markArrayNull
                            stloc instance
                            merge (emitPropertyWrapperDeserialization arrayMap.Wrapper)
                            ldloc instance
                            iif arrayMap.HasOptionalElement
                                (emit' {
                                    call (optionalSomeMethod.MakeGenericMethod([| arrayMap.Type |]))
                                })
                        })
                    })
                })
            })
        }

    let emitMatchType property =
        match property with
        | Some(Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap })
        | Some(Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }) ->
            emit' {
                ldarg_0
                call typeMap.Deserialization.MatchType
            }
        | None
        | Some(Individual { Element = None })
        | Some(Array { Element = None; ItemElement = None }) ->
            emit' {
                ldc_i4_0
            }
        | Some(Individual { Element = Some(name,_,_) })
        | Some(Array { Element = Some(name,_,_) })
        | Some(Array { Element = None; ItemElement = Some(name,_,_) }) ->
            emit' {
                ldarg_0
                callvirt_expr <@ (null: XmlReader).LocalName @>
                ldstr name.LocalName
                string_equals
                ldarg_0
                callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                ldstr name.NamespaceName
                string_equals
                add
                ldc_i4_2
                div
            }

    let emitPropertyDeserialization skipVar (prop: Property) =
        let content =
            match prop with
            | Individual propertyMap -> emitIndividualPropertyDeserialization true propertyMap
            | Array arrayMap -> emitArrayPropertyDeserialization true skipVar arrayMap
        emit' {
            merge content
            callvirt prop.SetMethod.Value
        }

    let emitSequenceDeserialization (startLabel: Label, returnLabel: Label) (skipRead: LocalBuilder, depthVar: LocalBuilder) (properties: Property list) =
        let rec emitPropertyDeser startLabel (propList: Property list) =
            match propList with
            | [] -> id
            | prop::xs ->
                let requiredProp = propList |> firstRequired
                let emitContent nextLabel =
                    let content =
                        match prop with
                        | Individual { Element = Some(name,_,isOptional) }
                        | Array { Element = Some(name,_,isOptional) } ->
                            emit' {
                                define_labels 2 (fun (List2(markDeserialize, markError)) -> emit' {
                                    // reader.LocalName != property.Name
                                    ldarg_0
                                    callvirt_expr <@ (null: XmlReader).LocalName @>
                                    ldstr name.LocalName
                                    string_equals
                                    brfalse markError
                                    ldarg_0
                                    callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                                    ldstr name.NamespaceName
                                    string_equals
                                    brtrue markDeserialize
                                    set_marker markError
                                    if_else isOptional
                                        (emit' {
                                            ldc_i4_1
                                            stloc skipRead
                                            br nextLabel
                                        })
                                        (emitWrongElementException (safe name) prop.Wrapper)
                                    set_marker markDeserialize
                                })
                            }
                        | _ -> id
                    emit' {
                        merge content
                        // Deserialize property
                        merge (emitPropertyDeserialization skipRead prop)
                        merge (emitPropertyDeser nextLabel xs)
                    }
                emit' {
                    set_marker startLabel
                    define_label (fun markSuccess2 -> emit' {
                        merge (emitMoveToEndOrNextElement markSuccess2 (skipRead, depthVar) prop.PropertyName)
                        if_some_none requiredProp
                            (fun p ->
                                let expectedName =
                                    match p with
                                    | Individual { Element = Some(name,_,_) } | Array { Element = Some(name,_,_) } | Array { ItemElement = Some(name,_,_) } -> safe name
                                    | _ -> "<end of sequence>"
                                emitWrongElementException expectedName prop.Wrapper)
                            (emit' { br returnLabel })
                        set_marker markSuccess2
                        nop
                        // reader.Depth != depth
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).Depth @>
                        ldloc depthVar
                        ceq
                        brfalse startLabel
                        // reader.NodeType != XmlNodeType.Element
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).NodeType @>
                        ldc_node_type XmlNodeType.Element
                        ceq
                        brfalse startLabel
                        if_else xs.IsEmpty
                            (emitContent returnLabel)
                            (emit' { define_label emitContent }) 
                    })
                }
        match properties with
        | [] ->
            emit' {
                set_marker startLabel
                nop
            }
        | _ -> emitPropertyDeser startLabel properties

let (|InlineContent|_|) (properties: Property list) =
    match properties with
    | [Individual({ Element = None; TypeMap = typeMap }) as prop]
    | [Array({ Element = None; ItemElement = None; ItemTypeMap = typeMap }) as prop] ->
        match typeMap.Layout with
        | Some(LayoutKind.Choice) -> None
        | _ -> Some(prop)
    | _ -> None

let rec private createDeserializeContentMethodBody (typeMap: TypeMap) (properties: Property list) =
    let emitContent skipRead returnLabel =
        match properties with
        | InlineContent(prop) ->
            emit' {
                ldc_i4_0
                stloc skipRead
                merge (EmitDeserialization.emitPropertyDeserialization skipRead prop)
            }
        | _ ->
            let emitLayoutContent startLabel returnLabel varDepth =
                match typeMap.Layout.Value with
                | LayoutKind.Choice -> id
                | LayoutKind.Sequence ->
                    let requiredProperty = properties |> firstRequired
                    emit' {
                        define_label (fun label -> emit' {
                            ldarg_0
                            callvirt_expr <@ (null: XmlReader).IsEmptyElement @>
                            if_some_none requiredProperty
                                (fun p ->
                                    let (name,_,_) = p.Element |> Option.get
                                    emit' {
                                        brfalse label
                                        merge (EmitDeserialization.emitWrongElementException (safe name) (Type typeMap))
                                    })
                                (emit' { brtrue returnLabel })
                            set_marker label
                            nop
                            merge (EmitDeserialization.emitSequenceDeserialization (startLabel, returnLabel) (skipRead, varDepth) properties)
                        })
                    }
                | _ -> failwith "Not implemented"
            emit' {
                declare_variable (lazy declareLocalOf<int>) (fun varDepth -> emit' {
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).Depth @>
                    stloc varDepth
                    define_label (fun startLabel -> emit' {
                        ldloc skipRead
                        brtrue startLabel
                        ldloc varDepth
                        ldc_i4_1
                        add
                        stloc varDepth
                        if_some typeMap.BaseType (fun typeMap -> emit' {
                            ldarg_0
                            ldarg_1
                            ldloc skipRead
                            ldarg_3
                            call typeMap.Deserialization.Content
                            stloc skipRead
                        })
                        merge (emitLayoutContent startLabel returnLabel varDepth)
                    })
                })
            }
    emit' {
        define_label (fun returnLabel -> emit' {
            declare_variable (lazy declareLocalOf<bool>) (fun skipRead -> emit' {
                ldarg_2
                stloc skipRead
                merge (emitContent skipRead returnLabel)
                set_marker returnLabel
                ldloc skipRead
                ret
            })
        })
    }

and createTypeSerializers isEncoded (typeMap: TypeMap) =
    let properties = getContentOfType typeMap |> getProperties (getTypeMap isEncoded)
    let directSubTypes = typeMap.Type |> findDirectSubTypes isEncoded

    #if PRINT_IL
    let typ = typeMap.Type
    fprintfn stream "--------------------- <%s root ser> ---------------------" typ.FullName
    #endif
    // Emit serializers
    defineMethod typeMap.Serialization.Root
        (EmitSerialization.emitRootSerializerMethod isEncoded directSubTypes typeMap)
    #if PRINT_IL
    fprintfn stream "--------------------- </%s root ser> ---------------------" typ.FullName
    #endif

    #if PRINT_IL
    fprintfn stream "--------------------- <%s content ser> ---------------------" typ.FullName
    #endif
    defineMethod typeMap.Serialization.Content
        (EmitSerialization.emitContentSerializerMethod isEncoded properties >> ret)
    #if PRINT_IL
    fprintfn stream "--------------------- </%s content ser> ---------------------" typ.FullName
    #endif

    let hasInlineContent = match properties with InlineContent _ -> true | _ -> false

    #if PRINT_IL
    fprintfn stream "--------------------- <%s root deser> ---------------------" typ.FullName
    #endif
    // Emit deserializers
    defineMethod typeMap.Deserialization.Root
        (EmitDeserialization.emitRootDeserializerMethod hasInlineContent directSubTypes typeMap)
    #if PRINT_IL
    fprintfn stream "--------------------- </%s root deser> ---------------------" typ.FullName
    #endif

    #if PRINT_IL
    fprintfn stream "--------------------- <%s content deser> ---------------------" typ.FullName
    #endif
    defineMethod typeMap.Deserialization.Content
        (createDeserializeContentMethodBody typeMap properties)
    #if PRINT_IL
    fprintfn stream "--------------------- </%s content deser> ---------------------" typ.FullName
    #endif

    match properties with
    | [Individual { Element = None }] | [Array { Element = None; ItemElement = None }] ->
        ()
    | _ ->
        #if PRINT_IL
        fprintfn stream "--------------------- <%s match deser> ---------------------" typ.FullName
        #endif
        defineMethod typeMap.Deserialization.MatchType
            (EmitDeserialization.emitMatchType (properties |> List.tryHead) >> ret)
        #if PRINT_IL
        fprintfn stream "--------------------- </%s match deser> ---------------------" typ.FullName
        #endif

and createChoiceTypeSerializers isEncoded (properties: Property list) (choiceMap: TypeMap) =
    let genSerialization () =
        let rec generate (conditionEnd: Label) (label: Label option) (properties: Property list) =
            match properties with
            | [] -> id
            | property::other ->
                let idField, id = match property.Wrapper with Choice(_, idField, _, id, _) -> idField, id | _ -> failwith "never"
                let nextBlock nextLabel = emit' {
                    brfalse nextLabel
                    nop
                    merge (EmitSerialization.emitOptionalFieldSerialization property (EmitSerialization.emitPropertyContentSerialization (EmitSerialization.emitPropertyValue property) isEncoded property))
                    nop
                    br conditionEnd
                    merge (generate conditionEnd (Some nextLabel) other)
                }
                emit' {
                    if_some label (fun label -> emit' {
                        set_marker label
                        nop
                    })
                    ldarg_1
                    castclass choiceMap.Type
                    ldfld idField
                    ldc_i4 id
                    ceq
                    if_else other.IsEmpty
                        (nextBlock conditionEnd)
                        (emit' { define_label nextBlock }) 
                }

        defineMethod choiceMap.Serialization.Root (emit' {
            define_label (fun conditionEnd -> emit' {
                merge (generate conditionEnd None properties)
                set_marker conditionEnd
                ret
            })
        })

    let genContentDeserialization () =
        defineMethod choiceMap.Deserialization.Content (loadArg2 >> ret)

    let genDeserialization () =
        let rec generate (skipRead: LocalBuilder) (markReturn: Label) (properties: Property list) =
            match properties with
            | [] -> id
            | property::other ->
                let mi = match property.Wrapper with Choice(_, _, _, _, mi) -> mi | _ -> failwith "never"

                let emitter (label: Label) =
                    match property with
                    | Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                    | Individual { Element = None; TypeMap = typeMap }
                    | Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                    | Array { Element = None; ItemElement = None; ItemTypeMap = typeMap } ->
                        emit' {
                            declare_variable (lazy (declareLocal property.Type)) (fun instance -> emit' {
                                ldarg_0
                                call typeMap.Deserialization.MatchType
                                brfalse label
                                newobj (typeMap.Type.GetConstructor([| |]))
                                stloc instance
                                ldarg_0
                                ldloc instance
                                ldloc skipRead
                                ldarg_1
                                call typeMap.Deserialization.Content
                                stloc skipRead
                                ldloc instance
                            })
                        }
                    | Individual { Element = Some(name,_,_) }
                    | Array { Element = Some(name,_,_) }
                    | Array { Element = None; ItemElement = Some(name,_,_) } ->
                        emit' {
                            ldarg_0
                            callvirt_expr <@ (null: XmlReader).LocalName @>
                            ldstr name.LocalName
                            string_equals
                            ldarg_0
                            callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                            ldstr name.NamespaceName
                            string_equals
                            add
                            ldc_i4_2
                            div
                            brfalse label
                            ldc_i4_0
                            stloc skipRead
                            merge (
                                match property with
                                | Individual propertyMap -> EmitDeserialization.emitIndividualPropertyDeserialization false propertyMap
                                | Array arrayMap -> EmitDeserialization.emitArrayPropertyDeserialization false skipRead arrayMap
                            )
                        }

                emit' {
                    define_label (fun label -> emit' {
                        merge (emitter label)
                        call mi
                        br markReturn
                        set_marker label
                        nop
                        merge (generate skipRead markReturn other)
                    })
                }

        defineMethod choiceMap.Deserialization.Root (emit' {
            define_label (fun markReturn -> emit' {
                declare_variable (lazy declareLocalOf<bool>) (fun skipRead ->
                    let names = properties |> List.map (fun p -> p.PropertyName)
                    let errorMessage = sprintf "Invalid message: expected one of %A, but `{0}` was found instead." names 
                    emit' {
                        ldc_i4_1
                        stloc skipRead
                        merge (generate skipRead markReturn properties)
                        ldstr errorMessage
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).LocalName @>
                        string_equals
                        newobj_expr <@ Exception("") @>
                        throw
                    }
                )
                set_marker markReturn
            })
            ret
        })

    let genMatch () =
        defineMethod choiceMap.Deserialization.MatchType (emit' {
            define_label (fun markReturn ->
                let rec generate (properties: Property list) =
                    match properties with
                    | [] ->
                        emit' {
                            ldc_i4_0
                            br markReturn
                        }
                    | property::other ->
                        emit' {
                            merge (EmitDeserialization.emitMatchType (Some property))
                            define_label (fun label -> emit' {
                                brfalse label
                                ldc_i4_1
                                br markReturn
                                set_marker label
                            })
                            nop
                            merge (generate other)
                        }
                emit' {
                    merge (generate properties)
                    set_marker markReturn
                })
            ret
        })

    genSerialization ()
    genContentDeserialization ()
    genDeserialization ()
    genMatch ()

and private getProperties (tmf: Type -> TypeMap) (input: PropertyInput list) : Property list =
    input
    |> List.map
        (fun (wrapper, propName, propertyType, isOptionalType, hasValueMethod, getMethod, setMethod, attr, cattr) ->
            let name = match attr.Name with null | "" -> propName | name -> name
            let xname = match attr.Namespace with "" -> XName.Get(name) | ns -> XName.Get(name, ns)
            let element = if attr.MergeContent then None else Some(xname, attr.IsNullable, isOptionalType)
            if propertyType.IsArray then
                let elementType = propertyType.GetElementType()
                let itemTypeMap = (if attr.UseXop then typeof<XopBinaryContent> else elementType) |> tmf
                let itemName = cattr |> Option.bind (fun a -> match a.ItemName with null | "" -> None | name -> Some(name)) |> MyOption.defaultWith (fun _ -> "item")
                let itemXName = cattr |> Option.bind (fun a -> match a.ItemNamespace with "" -> None | ns -> Some(XName.Get(itemName, ns))) |> MyOption.defaultWith (fun _ -> XName.Get(itemName))
                let itemElement = if itemTypeMap.Layout <> Some(LayoutKind.Choice)
                                  then if cattr.IsSome && cattr.Value.MergeContent then None else Some(itemXName, cattr.IsSome && cattr.Value.ItemIsNullable, true)
                                  else None
                Array { Type = propertyType
                        Element = element
                        ItemTypeMap = itemTypeMap
                        ItemElement = itemElement
                        ItemSimpleTypeName = XRoadHelper.getSystemTypeName elementType.FullName
                        Wrapper = wrapper
                        GetMethod = getMethod
                        SetMethod = setMethod
                        HasValueMethod = hasValueMethod }
            else
                let propertyTypeMap = (if attr.UseXop then typeof<XopBinaryContent> else propertyType) |> tmf
                let element = if propertyTypeMap.Layout <> Some(LayoutKind.Choice) then element else None
                Individual { TypeMap = propertyTypeMap
                             SimpleTypeName = XRoadHelper.getSystemTypeName (propertyType.FullName)
                             Element = element
                             Wrapper = wrapper
                             GetMethod = getMethod
                             SetMethod = setMethod
                             HasValueMethod = hasValueMethod })

and private typeMaps = ConcurrentDictionary<Type, TypeMap>()
and private uncompleteTypeMaps = ConcurrentDictionary<Type, TypeMap>()

and private createTypeMap (isEncoded: bool) (typ: Type) =
    let addTypeMap (init: TypeMap -> unit) (typ: Type) =
        let serialization, deserialization = typ |> Serialization.Create, typ |> Deserialization.Create
        let typeMap = TypeMap.Create(typ, deserialization, serialization, typ |> findBaseType isEncoded)
        if typeMaps.TryAdd(typ, typeMap) then
            uncompleteTypeMaps.TryAdd(typ, typeMap) |> ignore
            try
                typeMap |> init
            // with
            //     TODO: generate exceptions for invalid typemap methods.
            finally
                uncompleteTypeMaps.TryRemove(typ) |> ignore
                typeMap.IsComplete <- true
            typeMap
        else typeMaps.[typ]
    match typ with
    | NotSerializable ->
        failwithf "Type `%s` is not serializable." typ.FullName
    | Serializable(typeAttribute) ->
        typ |> addTypeMap (fun typeMap ->
            #if PRINT_IL
            fprintfn stream "===================== <%s> ==========================" typ.FullName
            #endif
            match typeAttribute.Layout with
            | LayoutKind.Choice -> typeMap |> createChoiceTypeSerializers isEncoded (getContentOfChoice typeMap |> getProperties (getTypeMap isEncoded))
            | _ -> typeMap |> createTypeSerializers isEncoded
            #if PRINT_IL
            fprintfn stream "===================== </%s> ==========================" typ.FullName
            stream.Flush()
            #endif
            )

and internal getTypeMap (isEncoded: bool) (typ: Type) : TypeMap =
    match typeMaps.TryGetValue(typ) with
    | true, typeMap -> typeMap
    | false, _ -> typ |> createTypeMap isEncoded

and findTypeMap isEncoded (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> None
    | _ -> Some(typ |> getTypeMap isEncoded)

and findBaseType isEncoded (typ: Type) =
    if typ.BaseType |> isNull || typ.BaseType = typeof<obj> then None
    else match typ.BaseType |> findTypeMap isEncoded with
         | None -> typ.BaseType |> findBaseType isEncoded
         | typeMap -> typeMap

and findDirectSubTypes (isEncoded: bool) (typ: Type) : TypeMap list =
    typ.Assembly.GetTypes()
    |> List.ofArray
    |> List.filter (fun x -> x.BaseType = typ)
    |> List.choose (findTypeMap isEncoded)

let getCompleteTypeMap isEncoded typ =
    let typeMap = getTypeMap isEncoded typ
    while uncompleteTypeMaps.Count > 0 do
        System.Threading.Thread.Sleep(100)
    typeMap

module internal XsdTypes =
    open NodaTime
    open NodaTime.Text
    open System.IO

    let serializeDefault (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(value)

    let serializeBigInteger (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(value.ToString())

    let serializeString (writer: XmlWriter, value: obj, _: SerializerContext) =
        if value |> isNull then writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        elif unbox value = "" then ()
        else writer.WriteValue(value)

    let serializeNullable (writer: XmlWriter) (value: obj) (context: SerializerContext) fser =
        if value |> isNull then writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        else fser(writer, value, context)

    let serializeLocalDate (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(LocalDatePattern.Iso.Format(unbox value))

    let serializeLocalDateTime (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(LocalDateTimePattern.GeneralIso.Format(unbox value))

    let deserializeNullable (reader: XmlReader) (context: SerializerContext) fdeser =
        let nilValue = reader.GetAttribute("nil", XmlNamespace.Xsi)
        let nilValue = if nilValue |> isNull then "" else nilValue.ToLower()
        if nilValue = "1" || nilValue = "true" then null else fdeser(reader, context)

    let deserializeValue<'T> (reader: XmlReader) (_: SerializerContext) (fread: unit -> 'T) : obj =
        if reader.IsEmptyElement then box Unchecked.defaultof<'T>
        elif reader.Read() then fread() |> box
        else failwith "Unexpected end of SOAP message."

    let deserializeStringValue (reader: XmlReader, _: SerializerContext) : obj =
        if reader.IsEmptyElement then box ""
        elif reader.Read() then reader.ReadContentAsString() |> box
        else failwith "Unexpected end of SOAP message."

    let deserializeDateTimeValue value =
        match LocalDateTimePattern.ExtendedIso.Parse(value) with
        | result when result.Success -> result.Value
        | _ -> OffsetDateTimePattern.ExtendedIso.Parse(value).GetValueOrThrow().LocalDateTime

    let serializeNullableDefault (writer, value, context) = serializeNullable writer value context serializeDefault
    let serializeNullableBigInteger (writer, value, context) = serializeNullable writer value context serializeBigInteger
    let serializeNullableLocalDate (writer, value, context) = serializeNullable writer value context serializeLocalDate
    let serializeNullableLocalDateTime (writer, value, context) = serializeNullable writer value context serializeLocalDateTime

    let deserializeBoolean (reader, context) = deserializeValue reader context reader.ReadContentAsBoolean
    let deserializeDecimal (reader, context) = deserializeValue reader context reader.ReadContentAsDecimal
    let deserializeDouble (reader, context) = deserializeValue reader context reader.ReadContentAsDouble
    let deserializeInt32 (reader, context) = deserializeValue reader context reader.ReadContentAsInt
    let deserializeInt64 (reader, context) = deserializeValue reader context reader.ReadContentAsLong
    let deserializeBigInteger (reader, context) = deserializeValue reader context (reader.ReadContentAsDecimal >> BigInteger)
    let deserializeLocalDate (reader, context) = deserializeValue reader context (fun () -> LocalDatePattern.Iso.Parse(reader.ReadContentAsString()).GetValueOrThrow())
    let deserializeLocalDateTime (reader, context) = deserializeValue reader context (fun () -> reader.ReadContentAsString() |> deserializeDateTimeValue)

    let deserializeNullableBoolean (reader, context) = deserializeNullable reader context deserializeBoolean
    let deserializeNullableDecimal (reader, context) = deserializeNullable reader context deserializeDecimal
    let deserializeNullableDouble (reader, context) = deserializeNullable reader context deserializeDouble
    let deserializeNullableInt32 (reader, context) = deserializeNullable reader context deserializeInt32
    let deserializeNullableInt64 (reader, context) = deserializeNullable reader context deserializeInt64
    let deserializeNullableBigInteger (reader, context) = deserializeNullable reader context deserializeBigInteger
    let deserializeNullableLocalDate (reader, context) = deserializeNullable reader context deserializeLocalDate
    let deserializeNullableLocalDateTime (reader, context) = deserializeNullable reader context deserializeLocalDateTime
    let deserializeString (reader, context) = deserializeNullable reader context deserializeStringValue

    let serializeBinaryContent (writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            let content = unbox<BinaryContent> value
            if context.IsMultipart then
                context.Attachments.Add(content.ContentID, content)
                writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            else
                let bytes = (unbox<BinaryContent> value).GetBytes()
                writer.WriteBase64(bytes, 0, bytes.Length)

    let serializeXopBinaryContent(writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            writer.WriteStartElement("xop", "Include", XmlNamespace.Xop)
            let content = unbox<BinaryContent> value
            context.Attachments.Add(content.ContentID, content)
            writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            writer.WriteEndElement()

    let deserializeBinaryContent (reader: XmlReader, context: SerializerContext) =
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            match reader.GetAttribute("href") with
            | null ->
                if reader.IsEmptyElement then BinaryContent.Create([| |])
                else
                    reader.Read() |> ignore
                    let bufferSize = 4096
                    let buffer = Array.zeroCreate<byte>(bufferSize)
                    use stream = new MemoryStream()
                    let rec readContents() =
                        let readCount = reader.ReadContentAsBase64(buffer, 0, bufferSize)
                        if readCount > 0 then stream.Write(buffer, 0, readCount)
                        if readCount = bufferSize then readContents()
                    readContents()
                    stream.Flush()
                    stream.Position <- 0L
                    BinaryContent.Create(stream.ToArray())
            | contentID -> context.GetAttachment(contentID)

    let deserializeXopBinaryContent (reader: XmlReader, context: SerializerContext) =
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            if reader.IsEmptyElement then BinaryContent.Create([| |])
            else
                let depth = reader.Depth + 1
                let rec moveToXopInclude () =
                    if reader.Read() then
                        if reader.NodeType = XmlNodeType.EndElement && reader.Depth < depth then false
                        elif reader.NodeType <> XmlNodeType.Element || reader.Depth <> depth || reader.LocalName <> "Include" || reader.NamespaceURI <> XmlNamespace.Xop then moveToXopInclude()
                        else true
                    else false
                if moveToXopInclude () then
                    match reader.GetAttribute("href") with
                    | null -> failwithf "Missing reference to multipart content in xop:Include element."
                    | contentID -> context.GetAttachment(contentID)
                else BinaryContent.Create([| |])

    let private addTypeMap typ ser deser =
        let typeMap = TypeMap.Create(typ, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
        typeMap.IsComplete <- true
        typeMaps.TryAdd(typ, typeMap) |> ignore

    let private addBinaryTypeMap typ ser deser =
        let typeMap = TypeMap.Create(typeof<BinaryContent>, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
        typeMap.IsComplete <- true
        typeMaps.TryAdd(typ, typeMap) |> ignore

    let mi e = match e with Call(_,mi,_) -> mi | _ -> failwith "do not use for that"

    let init () =
        addTypeMap typeof<bool> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeBoolean(null, null) @>)
        addTypeMap typeof<Nullable<bool>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableBoolean(null, null) @>)
        addTypeMap typeof<decimal> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDecimal(null, null) @>)
        addTypeMap typeof<Nullable<decimal>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDecimal(null, null) @>)
        addTypeMap typeof<double> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDouble(null, null) @>)
        addTypeMap typeof<Nullable<double>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDouble(null, null) @>)
        addTypeMap typeof<int32> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt32(null, null) @>)
        addTypeMap typeof<Nullable<int32>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt32(null, null) @>)
        addTypeMap typeof<int64> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt64(null, null) @>)
        addTypeMap typeof<Nullable<int64>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt64(null, null) @>)
        addTypeMap typeof<BigInteger> (mi <@ serializeBigInteger(null, null, null) @>) (mi <@ deserializeBigInteger(null, null) @>)
        addTypeMap typeof<Nullable<BigInteger>> (mi <@ serializeNullableBigInteger(null, null, null) @>) (mi <@ deserializeNullableBigInteger(null, null) @>)
        addTypeMap typeof<LocalDate> (mi <@ serializeLocalDate(null, null, null) @>) (mi <@ deserializeLocalDate(null, null) @>)
        addTypeMap typeof<Nullable<LocalDate>> (mi <@ serializeNullableLocalDate(null, null, null) @>) (mi <@ deserializeNullableLocalDate(null, null) @>)
        addTypeMap typeof<LocalDateTime> (mi <@ serializeLocalDateTime(null, null, null) @>) (mi <@ deserializeLocalDateTime(null, null) @>)
        addTypeMap typeof<Nullable<LocalDateTime>> (mi <@ serializeNullableLocalDateTime(null, null, null) @>) (mi <@ deserializeNullableLocalDateTime(null, null) @>)
        addTypeMap typeof<string> (mi <@ serializeString(null, null, null) @>) (mi <@ deserializeString(null, null) @>)
        addBinaryTypeMap typeof<BinaryContent> (mi <@ serializeBinaryContent(null, null, null) @>) (mi <@ deserializeBinaryContent(null, null) @>)
        addBinaryTypeMap typeof<XopBinaryContent> (mi <@ serializeXopBinaryContent(null, null, null) @>) (mi <@ deserializeXopBinaryContent(null, null) @>)

module internal DynamicMethods =
    let requiredOpAttr<'T when 'T :> Attribute and 'T : null and 'T : equality> (mi: MethodInfo) : 'T =
        mi.GetCustomAttribute<'T>()
        |> Option.ofObj
        |> MyOption.defaultWith (fun _ -> failwithf "Operation should define `%s`." typeof<'T>.Name)
        
    let emitDeserializer (mi: MethodInfo) (responseAttr: XRoadResponseAttribute) : DeserializerDelegate =
        let returnType = responseAttr.ReturnType |> Option.ofObj |> MyOption.defaultValue mi.ReturnType
        let typeMap = getCompleteTypeMap responseAttr.Encoded returnType
        typeMap.DeserializeDelegate.Value
        
    let emitSerializer (mi: MethodInfo) (requestAttr: XRoadRequestAttribute) : OperationSerializerDelegate =
        let method = 
            DynamicMethod
                ( sprintf "serialize_%s" mi.Name,
                  null,
                  [| typeof<XmlWriter>; typeof<obj[]>; typeof<SerializerContext> |],
                  true )

        let parameters = getContentOfMethod mi |> getProperties (getCompleteTypeMap requestAttr.Encoded)
        method.GetILGenerator() |> (emit' {
            ldarg_0
            ldstr requestAttr.Name
            ldstr requestAttr.Namespace
            callvirt_expr <@ (null: XmlWriter).WriteStartElement("", "") @>
            merge (EmitSerialization.emitContentSerializerMethod requestAttr.Encoded parameters)
            ldarg_0
            callvirt_expr <@ (null: XmlWriter).WriteEndElement() @>
            ret
        }) |> ignore

        method.CreateDelegate(typeof<OperationSerializerDelegate>) |> unbox
        
    let createMethodMap (mi: MethodInfo) : MethodMap =
        let operationAttr = mi |> requiredOpAttr<XRoadOperationAttribute>
        let requestAttr = mi |> requiredOpAttr<XRoadRequestAttribute>
        let responseAttr = mi |> requiredOpAttr<XRoadResponseAttribute>
        let requiredHeadersAttr = mi.GetCustomAttribute<XRoadRequiredHeadersAttribute>() |> Option.ofObj

        { Deserializer = emitDeserializer mi responseAttr
          Serializer = emitSerializer mi requestAttr
          Protocol = operationAttr.Protocol
          Request =
            { IsEncoded = requestAttr.Encoded
              IsMultipart = requestAttr.Multipart
              Accessor = Some(XmlQualifiedName(requestAttr.Name, requestAttr.Namespace)) }
          Response =
            { IsEncoded = responseAttr.Encoded
              IsMultipart = responseAttr.Multipart
              Accessor = Some(XmlQualifiedName(responseAttr.Name, responseAttr.Namespace)) }
          ServiceCode = operationAttr.ServiceCode
          ServiceVersion = operationAttr.ServiceVersion |> Option.ofObj
          Namespaces = []
          RequiredHeaders = dict [ match requiredHeadersAttr with
                                   | Some(attr) -> yield (attr.Namespace, attr.Names)
                                   | None -> () ] }

    let private methodMaps = ConcurrentDictionary<MethodInfo, MethodMap>()

    let internal getMethodMap mi =
        match methodMaps.TryGetValue(mi) with
        | true, mm -> mm
        | _ -> methodMaps.GetOrAdd(mi, (createMethodMap mi))

let getMethodMap = DynamicMethods.getMethodMap

do XsdTypes.init()
