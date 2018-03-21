﻿module internal XRoad.Emitter

open Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.Serialization.Attributes
open EmitterDsl
open XmlExtensions

#if NET40
type ICustomAttributeProvider with
    member this.GetCustomAttribute<'T when 'T :> Attribute and 'T : null>() =
        match this.GetCustomAttributes(typeof<'T>, false) with
        | [||] -> null
        | [|attr|] -> attr |> unbox<'T>
        | _ -> failwith "multiple attributes of same type"
    member this.GetCustomAttributes<'T when 'T :> Attribute and 'T : null>() =
        this.GetCustomAttributes(typeof<'T>, false) |> Array.map unbox<'T>

type Type with
    member this.GenericTypeArguments with get() = this.GetGenericArguments()

type MethodInfo with
    member this.CreateDelegate(delegateType: Type) =
        (this |> unbox<DynamicMethod>).CreateDelegate(delegateType)
#endif

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
              Content = DynamicMethod(sprintf "%s_DeserializeContent" typ.FullName, null, [| typeof<XmlReader>; typeof<obj>; typeof<int>; typeof<SerializerContext> |], true)
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
    member this.Description =
        match this with
        | Choice _ -> sprintf "choice `%s`" this.Name
        | Method _ -> sprintf "operation `%s` wrapper element" this.Name
        | Type _ -> sprintf "type `%s`" this.Name

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

type DatePlaceholder = struct end

let getDefaultSystemTypeName = function
| "NodaTime.LocalDate" -> Some(XmlQualifiedName("date", XmlNamespace.Xsd))
| "NodaTime.LocalDateTime" -> Some(XmlQualifiedName("dateTime", XmlNamespace.Xsd))
| "System.String" -> Some(XmlQualifiedName("string", XmlNamespace.Xsd))
| "System.Boolean" -> Some(XmlQualifiedName("boolean", XmlNamespace.Xsd))
| "System.DateTime" -> Some(XmlQualifiedName("dateTime", XmlNamespace.Xsd))
| "System.Decimal" -> Some(XmlQualifiedName("decimal", XmlNamespace.Xsd))
| "System.Double" -> Some(XmlQualifiedName("double", XmlNamespace.Xsd))
| "System.Float" -> Some(XmlQualifiedName("float", XmlNamespace.Xsd))
| "System.Int32" -> Some(XmlQualifiedName("int", XmlNamespace.Xsd))
| "System.Numerics.BigInteger" -> Some(XmlQualifiedName("integer", XmlNamespace.Xsd))
| "System.Int64" -> Some(XmlQualifiedName("long", XmlNamespace.Xsd))
| _ -> None

let applyDataType (attr: XRoadElementAttribute) xmlType =
    if attr.Hint.HasFlag(SerializationHint.IsDate)
    then Some(XmlQualifiedName("date", XmlNamespace.Xsd)) 
    else xmlType

let firstRequired (properties: Property list) =
    properties
    |> List.tryPick (fun p -> match p.Element with Some(_,_,false) -> Some(p) | _ -> None)

type private XopBinaryContent() =
    inherit BinaryContent("", Data [| |])

let (|Serializable|NotSerializable|) (typ: Type) =
    let typName = match Nullable.GetUnderlyingType(typ) with null -> typ.FullName | typ -> typ.FullName
    match typName, typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | "NodaTime.LocalDateTime", null
    | "NodaTime.LocalDate", null -> Serializable(None)
    | _, null -> NotSerializable
    | _, attr -> Serializable(Some(attr))

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
                if p.PropertyType.IsGenericType && p.PropertyType.GetGenericTypeDefinition() = Opt.optionType.Value then
                    p.PropertyType.GenericTypeArguments.[0], true, Some(p.PropertyType.GetProperty("HasValue").GetGetMethod())
                else p.PropertyType, false, None
            Type typeMap, p.Name, propertyType, isOptionalType, hasValueMethod, Some(p.GetGetMethod()), Some(p.GetSetMethod(true)), attr, p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj)

let getContentOfMethod (mi: MethodInfo) : PropertyInput list =
    mi.GetParameters()
    |> Seq.choose (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj |> Option.map (fun x -> (p, x)))
    |> Seq.mapi (fun i (p, attr) ->
        let parameterType, isOptionalType, hasValueMethod =
            if p.ParameterType.IsGenericType && p.ParameterType.GetGenericTypeDefinition() = Opt.optionType.Value then
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
                if typ.IsGenericType && typ.GetGenericTypeDefinition() = Opt.optionType.Value then
                    typ.GenericTypeArguments.[0], true, Some(typ.GetProperty("HasValue").GetGetMethod())
                else typ, false, None
            let collectionAttr = choiceType.GetCustomAttributes<XRoadCollectionAttribute>() |> Seq.tryFind (fun a -> a.Id = attr.Id)
            Choice (choiceMap, idField, valueField, attr.Id, mi), attr.Name, parameterType, isOptionalType, hasValueMethod, None, None, attr, collectionAttr)
    |> Seq.toList

let emitDefaultValue typ = emit' {
    declare_variable (lazy declareLocal typ) (fun x -> emit' {
        ldloca x
        initobj typ
        ldloc x
    })
}

module EmitSerialization =
    /// Check if values type matches expected type.
    let private emitValueTypeTest (expectedType: Type) = emit' {
        ldarg_1
        call_expr <@ (null: obj).GetType() @>
        callvirt_expr <@ (null: Type).FullName @>
        ldstr expectedType.FullName
        string_equals
    }

    /// Write type attribute according to TypeMap.
    let emitTypeAttribute (typeName: string) (typeNamespace: string option) = emit' {
        ldarg_0
        ldstr "type"
        ldstr XmlNamespace.Xsi
        callvirt_expr <@ (null: XmlWriter).WriteStartAttribute("", "") @>
        nop
        ldarg_0
        ldstr typeName
        merge (
            match typeNamespace with
            | Some(ns) ->
                emit' {
                    ldstr ns
                    callvirt_expr <@ (null: XmlWriter).WriteQualifiedName("", "") @>
                }
            | None ->
                emit' {
                    callvirt_expr <@ (null: XmlWriter).WriteString("") @>
                }
        )
        nop
        ldarg_0
        callvirt_expr <@ (null: XmlWriter).WriteEndAttribute() @>
        nop
    }

    /// Emit type (and its base types) content serialization.
    let rec private emitContentSerialization (typeMap: TypeMap) = emit' {
        merge (match typeMap.BaseType with Some(tm) -> emitContentSerialization tm | None -> id)
        ldarg_0
        ldarg_1
        ldarg_2
        call typeMap.Serialization.Content
        nop
    }

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (typeMap: TypeMap) = emit' {
        ldstr (sprintf "Cannot serialize abstract type `%s`." typeMap.FullName)
        newobj_expr <@ Exception("") @>
        throw
    }

    /// Emit whole contents of TypeMap serialization.
    let private emitBodySerialization addType (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then emitAbstractTypeException typeMap else emit' {
            merge (if addType then emitTypeAttribute typeMap.Name typeMap.Namespace else id)
             // TODO : Attributes
            merge (emitContentSerialization typeMap)
        }

    /// Emit serialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchySerialization (markReturn: Label) isEncoded (subTypes: TypeMap list) typeMap =
        match subTypes with
        | [] ->
            emitBodySerialization isEncoded typeMap
        | subType::other ->
            emit' {
                define_label (fun markNext -> emit' {
                    // Check if type matches current TypeMap.
                    merge (emitValueTypeTest subType.Type)
                    brfalse markNext
                    merge (emitBodySerialization true subType)
                    br markReturn
                    set_marker markNext
                })
                nop
                merge (emitTypeHierarchySerialization markReturn isEncoded other typeMap)
            }

    let emitNilAttribute (markReturn: Label) = emit' {
        define_label (fun markNotNull -> emit' {
            ldnull
            ceq
            brfalse markNotNull
            ldarg_0
            ldstr "nil"
            ldstr XmlNamespace.Xsi
            ldstr "true"
            callvirt_expr <@ (null: XmlWriter).WriteAttributeString("", "", "") @>
            nop
            br markReturn
            set_marker markNotNull
        })
        nop
    }

    /// Emit root type serialization logic for given TypeMap.
    let emitRootSerializerMethod isEncoded (subTypes: TypeMap list) (typeMap: TypeMap) = emit' {
        define_label (fun markReturn -> emit' {
            // When value is `null`, write `xsi:nil` attribute and return.
            ldarg_1
            merge (emitNilAttribute markReturn)
            // Serialize value according to its type.
            merge (emitTypeHierarchySerialization markReturn isEncoded subTypes typeMap)
            set_marker markReturn
        })
        // Return
        ret
    }

    /// Provides value for array item at current index.
    let private emitArrayItemValue (array: LocalBuilder) (index: LocalBuilder) (typ: Type) = emit' {
        ldloc array
        ldloc index
        ldelem typ
        merge (if typ.IsValueType then emit' { box typ } else id)
    }

    /// Emit validation for not nullable types to have value specified.
    let private emitNotNullableCheck (name: string) emitValue property =
        match property with
        | Array _
        | Individual { TypeMap = { CanHaveNullAsValue = true } } ->
            emit' {
                define_label (fun markSuccess -> emit' {
                    // Check if value is null.
                    merge (emitValue property.Type)
                    ldnull
                    ceq
                    brfalse markSuccess
                    // Not nullable shouldn't have null as value, so throw exception.
                    ldstr (sprintf "Not nullable property `%s` of type `%s` has null value." name property.Wrapper.Name)
                    newobj_expr <@ Exception("") @>
                    throw
                    set_marker markSuccess
                })
                nop
            }
        | _ -> id

    let emitPropertyWrapperSerialization (property: Property) =
        match property.Wrapper with
        | Choice (_,_,fld,_,_) ->
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> MyOption.defaultWith (fun _ -> property.Type)
            emit' {
                ldarg_1
                castclass fld.DeclaringType
                ldfld fld
                merge (if property.Type.IsValueType then emit' { unbox ty } else emit' { castclass ty })
            }
        | Method (_, i) ->
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> MyOption.defaultWith (fun _ -> property.Type)
            emit' {
                ldarg_1
                ldc_i4 i
                ldelem_ref
                merge (if ty.IsValueType then emit' { unbox ty } else emit' { castclass ty })
            }
        | Type tm ->
            emit' {
                ldarg_1
                castclass tm.Type
                callvirt property.GetMethod.Value
            }

    let emitOptionalFieldSerialization (property: Property) emitContent =
        if not property.HasOptionalElement then emitContent else
        emit' {
            merge (emitPropertyWrapperSerialization property)
            declare_variable (lazy (declareLocal property.HasValueMethod.Value.DeclaringType)) (fun optionalType -> emit' {
                stloc optionalType
                ldloca optionalType
                call property.HasValueMethod.Value
                define_label (fun endContentLabel -> emit' {
                    brfalse endContentLabel
                    nop
                    merge emitContent
                    set_marker endContentLabel
                })
                nop
            })
        }

    /// Emit single property content serialization.
    let rec emitPropertyContentSerialization emitValue isEncoded (property: Property) : ILGenerator -> ILGenerator =
        // Write start element of the propery if its not merged with content.
        let writeStartElement =
            match property.Element with
            | Some(name, isNullable, _) ->
                emit' {
                    ldarg_0
                    ldstr name.LocalName
                    ldstr name.NamespaceName
                    callvirt_expr <@ (null: XmlWriter).WriteStartElement("", "") @>
                    nop
                    merge (if isNullable then id else emitNotNullableCheck name.LocalName emitValue property)
                    merge (
                        match isEncoded, property.SimpleTypeName with
                        | true, Some(typeName) -> emitTypeAttribute typeName.Name (Some(typeName.Namespace))
                        | _ -> id
                    )
                }
            | None -> id

        // Serialize property content value according to its TypeMap.
        let writePropertyContent = emit' {
            define_label (fun markReturn -> emit' {
                merge(
                    match property with
                    | Individual propertyMap ->
                        emit' {
                            ldarg_0
                            merge (emitValue propertyMap.TypeMap.Type)
                            ldarg_2
                            call propertyMap.TypeMap.Serialization.Root
                            nop
                        }
                    | Array arrayMap ->
                        let itemPropertyMap = Individual (arrayMap.GetItemPropertyMap())
                        emit' {
                            merge (emitValue arrayMap.Type)
                            merge (
                                match property.Element with
                                | Some(_) ->
                                    emitNilAttribute markReturn
                                | None -> 
                                    emit' {
                                        ldnull
                                        ceq
                                        brtrue markReturn
                                    }
                            )
                            merge (emitValue arrayMap.Type)
                            declare_variable (lazy (declareLocal arrayMap.Type)) (fun arr -> emit' {
                                stloc arr
                                declare_variable (lazy declareLocalOf<int>) (fun i -> emit' {
                                    ldc_i4_0
                                    stloc i
                                    define_labels_2 (fun markLoopCondition markLoopStart -> emit' {
                                        br markLoopCondition
                                        set_marker markLoopStart
                                        nop
                                        merge (emitPropertyContentSerialization (emitArrayItemValue arr i) isEncoded itemPropertyMap)
                                        ldloc i
                                        ldc_i4_1
                                        add
                                        stloc i
                                        set_marker markLoopCondition
                                        ldloc i
                                        ldloc arr
                                        ldlen
                                        conv_i4
                                        clt
                                        brtrue markLoopStart
                                    })
                                })
                            })
                        }
                )
                set_marker markReturn
                nop
            })
        }

        // Write end element if required.
        let writeEndElement =
            match property.Element with
            | Some(_) ->
                emit' {
                    ldarg_0
                    callvirt_expr <@ (null: XmlWriter).WriteEndElement() @>
                    nop
                }
            | None -> id

        writeStartElement >> writePropertyContent >> writeEndElement

    /// Unbox property value into correct type.
    let emitPropertyValue (property: Property) (typ: Type) = emit' {
        merge (emitPropertyWrapperSerialization property)
        merge (
            if not property.HasOptionalElement then id else emit' {
                declare_variable (lazy (declareLocal property.HasValueMethod.Value.DeclaringType)) (fun opt -> emit' {
                    stloc opt
                    ldloca opt
                    merge (
                        if not typ.IsValueType then emit' { ldnull } else (emitDefaultValue typ)
                    )
                    call (property.HasValueMethod.Value.DeclaringType.GetMethod("ValueOr", [| typ |]))
                })
            }
        )
        merge (if typ.IsValueType then emit' { box typ } else id)
    }

    /// Emit IL which serializes each property value into corresponding xml fragment.
    let emitContentSerializerMethod isEncoded (properties: Property list) =
        properties
        |> List.map (fun property -> emitOptionalFieldSerialization property (emitPropertyContentSerialization (emitPropertyValue property) isEncoded property))
        |> (fun items -> if items.IsEmpty then id else items |> List.reduce ((>>)))
        
    let emitSerializeNullable fser = emit' {
        define_labels_2 (fun lbl1 lbl2 -> emit' {
            ldarg_1
            ldnull
            ceq
            brtrue lbl1
            merge fser
            br lbl2
            set_marker lbl1
            ldarg_0
            ldstr "nil"
            ldstr XmlNamespace.Xsi
            ldstr "true"
            callvirt_expr <@ (null: XmlWriter).WriteAttributeString("", "", "") @>
            set_marker lbl2
        })
    }

module EmitDeserialization =
    let emitDebug arg = emit' {
        ldstr (sprintf "%s: {3}: <{1}:{0}> [{2}]" arg)
        ldarg_0
        callvirt_expr <@ (null: XmlReader).LocalName @>
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NamespaceURI @>
        ldarg_0
        callvirt_expr <@ (null: XmlReader).Depth @>
        box typeof<int>
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NodeType @>
        box typeof<int>
        call_expr <@ Console.WriteLine("", "", "", "", "") @>
    }

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
        merge (if typeMap.IsAnonymous then (emit' { ldc_i4_1 }) else (emit' { ldc_i4_0 }))
        merge (if isDefault then (emit' { ldc_i4_1 }) else (emit' { ldc_i4_0 }))
        call_expr <@ (null: XmlReader).IsQualifiedTypeName(null, "", "", false, false) @>
    }
    
    let emitXmlReaderRead = emit' {
        ldarg_0
        callvirt_expr <@ (null: XmlReader).Read() @>
        pop
    }

    /// Emit type (and its base types) content deserialization.
    let rec private emitContentDeserialization hasInlineContent (instance: LocalBuilder, depthVar: LocalBuilder) (typeMap: TypeMap) = emit' {
        merge (
            if hasInlineContent then id else
            emit' {
                ldarg_0
                callvirt_expr <@ (null: XmlReader).IsEmptyElement @>
                define_label (fun skipRead -> emit' {
                    brtrue skipRead
                    merge emitXmlReaderRead
                    set_marker skipRead
                })
            }
        )
        ldarg_0
        ldloc instance
        ldloc depthVar
        merge (
            if hasInlineContent then id else emit' {
                ldc_i4_1
                add
            }
        )
        ldarg_1
        call typeMap.Deserialization.Content
    }

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (typeMap: TypeMap) = emit' {
        ldstr (sprintf "Cannot deserialize abstract type `%s`." typeMap.FullName)
        newobj_expr <@ Exception("") @>
        throw
    }

    let emitWrongElementException expectedValue wrapper =
        let wrapperName =
            match wrapper with
            | Choice _ -> sprintf "choice `%s`" wrapper.Name
            | Method _ -> sprintf "operation `%s` wrapper element" wrapper.Name
            | Type _ -> sprintf "type `%s`" wrapper.Name
        emit' {
            ldstr (sprintf "Element `%s` was expected in subsequence of %s, but element `{0}` was found instead." expectedValue wrapperName)
            ldarg_0
            callvirt_expr <@ (null: XmlReader).LocalName @>
            call_expr <@ String.Format("", "") @>
            newobj_expr <@ Exception("") @>
            throw
        }

    /// Emit whole contents of TypeMap deserialization.
    let private emitBodyDeserialization hasInlineContent (depthVar: LocalBuilder) (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then emitAbstractTypeException typeMap else
        emit' {
            newobj (typeMap.Type.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public, null, [| |], [| |]))
            declare_variable (lazy (declareLocal typeMap.Type)) (fun instance -> emit' {
                stloc instance
                // TODO : Attributes
                merge (emitContentDeserialization hasInlineContent (instance, depthVar) typeMap)
                ldarg_0
                ldstr typeMap.Name
                ldstr (typeMap.Namespace |> MyOption.defaultValue "")
                ldloc depthVar
                ldc_i4_0
                call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                ldloc instance
            })
        }

    /// Emit deserialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchyDeserialization hasInlineContent (markReturn: Label) (depthVar: LocalBuilder) (subTypes: TypeMap list) qualifiedName typeMap =
        match subTypes with
        | [] ->
            let context = if typeMap.IsAnonymous then "anonymous type" else sprintf "type `%s`" (XName.Get(typeMap.Name, typeMap.Namespace |> MyOption.defaultValue "") |> safe)
            emit' {
                define_label (fun errorLabel -> emit' {
                    merge (emitValueTypeTest true qualifiedName typeMap)
                    brfalse errorLabel
                    merge (emitBodyDeserialization hasInlineContent depthVar typeMap)
                    br markReturn
                    set_marker errorLabel
                })
                ldstr (sprintf "Unexpected type value: using type `{0}` is not allowed in the context of %s." context)
                ldloc qualifiedName
                call_expr <@ String.Format("", "") @>
                newobj_expr <@ Exception("") @>
                throw
            }
        | subType::other ->
            emit' {
                define_label (fun markNext -> emit' {
                    merge (emitValueTypeTest false qualifiedName subType)
                    brfalse markNext
                    merge (emitBodyDeserialization hasInlineContent depthVar subType)
                    br markReturn
                    set_marker markNext
                })
                nop
                merge (emitTypeHierarchyDeserialization hasInlineContent markReturn depthVar other qualifiedName typeMap)
            }

    let emitRootDeserializerMethod hasInlineContent (subTypes: TypeMap list) (typeMap: TypeMap) = emit' {
        declare_variable (lazy declareLocalOf<int>) (fun depthVar -> emit' {
            ldarg_0
            callvirt_expr <@ (null: XmlReader).Depth @>
            stloc depthVar
            define_label (fun markReturn -> emit' {
                merge emitNullCheck
                define_label (fun label -> emit' {
                    brfalse label
                    ldarg_0
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).LocalName @>
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).Depth @>
                    ldc_i4_0
                    call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                    ldnull
                    br markReturn
                    set_marker label
                })
                declare_variable (lazy declareLocalOf<XmlQualifiedName>) (fun qualifiedName -> emit' {
                    merge emitTypeAttributeRead
                    stloc qualifiedName
                    merge (emitTypeHierarchyDeserialization hasInlineContent markReturn depthVar subTypes qualifiedName typeMap)
                })
                set_marker markReturn
            })
        })
        ret
    }

    let emitPropertyValueDeserialization (isContent: bool) (typeMap: TypeMap) = emit' {
        ldarg_0
        merge (if isContent then (emit' { ldarg_3 }) else (emit' { ldarg_1 }))
        call typeMap.Deserialization.Root
        merge (if typeMap.Type.IsValueType then (emit' { unbox typeMap.Type }) else (emit' { castclass typeMap.Type }))
    }

    let emitPropertyWrapperDeserialization (wrapper: ContentWrapper) =
        match wrapper with
        | Choice _ -> id
        | Method _ -> failwith "Method signature is not deserialzable"
        | Type tm -> emit' {
            ldarg_1
            castclass tm.Type
        }

    let emitIndividualPropertyDeserialization isContent (propertyMap: PropertyMap) = emit' {
        merge (emitPropertyValueDeserialization isContent propertyMap.TypeMap)
        declare_variable (lazy (declareLocal propertyMap.TypeMap.Type)) (fun x -> emit' {
            stloc x
            merge (emitPropertyWrapperDeserialization propertyMap.Wrapper)
            ldloc x
            merge (
                if propertyMap.HasOptionalElement then (emit' {
                    call (Opt.optionalSomeMethod.Value.MakeGenericMethod([| propertyMap.TypeMap.Type |]))
                }) else id
            )
        })
    }

    let emitArrayItemDeserialization isContent (arrayMap: ArrayMap, listInstance: LocalBuilder, markEnd: Label, stopIfWrongElement) =
        match arrayMap.ItemElement with
        | Some(name, _, _) ->
            emit' {
                ldarg_0
                ldstr name.LocalName
                ldstr name.NamespaceName
                call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
                define_label (fun deserializeLabel -> emit' {
                    brtrue deserializeLabel
                    merge (
                        if stopIfWrongElement then (emit' { br markEnd }) else (emit' {
                            ldstr "Unexpected element: found `{0}`, but was expecting to find `{1}`."
                            ldarg_0
                            callvirt_expr <@ (null: XmlReader).LocalName @>
                            ldstr (safe name)
                            call_expr <@ String.Format("", "", "") @>
                            newobj_expr <@ Exception("") @>
                            throw
                        })
                    )
                    set_marker deserializeLabel
                    ldloc listInstance
                    merge (emitPropertyValueDeserialization isContent arrayMap.ItemTypeMap)
                    callvirt (listInstance.LocalType.GetMethod("Add", [| arrayMap.ItemTypeMap.Type |]))
                })
            }
        | None -> id

    /// Emits array type deserialization logic.
    let emitArrayPropertyDeserialization isContent arrDepthVar (arrayMap: ArrayMap) =
        let listType = typedefof<ResizeArray<_>>.MakeGenericType(arrayMap.ItemTypeMap.Type)
        emit' {
            ldloc arrDepthVar
            declare_variable (lazy declareLocalOf<int>) (fun depthVar -> emit' {
                stloc depthVar
                define_labels_3 (fun markArrayNull markArrayEnd markLoopStart -> emit' {
                    merge (
                        if arrayMap.Element.IsSome then (emit' {
                            merge emitNullCheck
                            define_label (fun label -> emit' {
                                brfalse label
                                ldarg_0
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).LocalName @>
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).Depth @>
                                ldc_i4_0
                                call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                                ldnull
                                br markArrayNull
                                set_marker label
                            })
                            ldloc depthVar
                            ldc_i4_1
                            add
                            stloc depthVar
                        }) else id
                    )
                    declare_variable (lazy (declareLocal listType)) (fun listInstance -> emit'{
                        newobj (listType.GetConstructor([||]))
                        stloc listInstance
                        merge (
                            if arrayMap.Element.IsSome then (emit' {
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).IsEmptyElement @>
                                brtrue markArrayEnd
                                merge emitXmlReaderRead
                            }) else id
                        )
                        set_marker markLoopStart
                        ldarg_0
                        ldloc depthVar
                        call_expr <@ (null: XmlReader).FindNextStartElement(0) @>
                        brfalse markArrayEnd
                        merge (emitArrayItemDeserialization isContent (arrayMap, listInstance, markArrayEnd, arrayMap.Element.IsNone))
                        br markLoopStart
                        set_marker markArrayEnd
                        merge (
                            match arrayMap.Element with
                            | Some(name, _, _) ->
                                emit' {
                                    ldarg_0
                                    ldstr name.LocalName
                                    ldstr name.NamespaceName
                                    ldloc arrDepthVar
                                    ldc_i4_0
                                    call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                                }
                            | None -> id
                        )
                        declare_variable (lazy (declareLocal arrayMap.Type)) (fun instance -> emit' {
                            ldloc listInstance
                            callvirt (listType.GetMethod("ToArray", [| |]))
                            set_marker markArrayNull
                            stloc instance
                            merge (emitPropertyWrapperDeserialization arrayMap.Wrapper)
                            ldloc instance
                            merge (
                                if arrayMap.HasOptionalElement then (emit' {
                                    call (Opt.optionalSomeMethod.Value.MakeGenericMethod([| arrayMap.Type |]))
                                }) else id
                            )
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
                ldstr name.LocalName
                ldstr name.NamespaceName
                call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
            }

    let emitPropertyDeserialization depthVar property = emit' {
        merge (
            match property with
            | Individual propertyMap -> emitIndividualPropertyDeserialization true propertyMap
            | Array arrayMap -> emitArrayPropertyDeserialization true depthVar arrayMap
        )
        callvirt property.SetMethod.Value
    }

    let rec emitSequenceDeserialization (startLabel: Label) (returnLabel: Label) (depthVar: LocalBuilder) (properties: Property list) =
        let nextRequired = properties |> firstRequired
        match properties with
        | [] ->
            emit' {
                set_marker startLabel
                nop
            }
        | property::properties ->
            emit' {
                set_marker startLabel

                ldarg_0
                ldloc depthVar
                call_expr <@ (null: XmlReader).FindNextStartElement(0) @>

                merge (
                    match nextRequired with
                    | Some(rp) ->
                        emit' {
                            define_label (fun found -> emit' {
                                brtrue found
                                merge (emitWrongElementException (safe (rp.PropertyName.Value)) rp.Wrapper)
                                set_marker found
                            })
                        }
                    | None -> emit' { brfalse returnLabel }
                )

                define_labels_2 (fun markNext markDeserialize -> emit' {
                    merge (
                        match property with
                        | Individual { Element = Some(name,_,isOptional) }
                        | Array { Element = Some(name,_,isOptional) } ->
                            emit' {
                                ldarg_0
                                ldstr name.LocalName
                                ldstr name.NamespaceName
                                call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
                                brtrue markDeserialize
                                merge (if isOptional then (emit' { br markNext }) else (emitWrongElementException (safe name) property.Wrapper))
                                set_marker markDeserialize
                            }
                        | _ -> id
                    )

                    merge (emitPropertyDeserialization depthVar property)
                    merge (emitSequenceDeserialization markNext returnLabel depthVar properties)
                })
            }

    let emitReadToNextWrapper typ f = emit' {
        declare_variable (lazy declareLocalOf<int32>) (fun depth -> emit' {
            ldarg_0
            callvirt_expr <@ (null: XmlReader).Depth @>
            stloc depth
            declare_variable (lazy declareLocalOf<string>) (fun name -> emit' {
                ldarg_0
                callvirt_expr <@ (null: XmlReader).LocalName @>
                stloc name
                declare_variable (lazy declareLocalOf<string>) (fun ns -> emit' {
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                    stloc ns
                    declare_variable (lazy declareLocal typ) (fun v -> emit' {
                        merge f
                        stloc v
                        ldarg_0
                        ldloc name
                        ldloc ns
                        ldloc depth
                        ldc_i4_0
                        call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                        ldloc v
                    })
                })
            })
        })
    }

    let emitDeserializeValue (typ: Type) f = emit' {
        ldarg_0
        callvirt_expr <@ (null: XmlReader).IsEmptyElement @>
        define_labels_3 (fun lbl1 lbl2 lbl3 -> emit' {
            brfalse lbl1
            merge (emitDefaultValue typ)
            br lbl3
            set_marker lbl1
            ldarg_0
            callvirt_expr <@ (null: XmlReader).Read() @>
            brfalse lbl2
            merge f
            br lbl3
            set_marker lbl2
            ldstr "Unexpected end of SOAP message."
            newobj_expr <@ Exception("") @>
            throw
            set_marker lbl3
        })
    }

    let emitDeserializeNullable (typ: Type) fdeser = emit' {
        ldarg_0
        ldstr "nil"
        ldstr XmlNamespace.Xsi
        callvirt_expr <@ (null: XmlReader).GetAttribute("", "") @>
        declare_variable (lazy declareLocalOf<string>) (fun nilValue -> emit' {
            stloc nilValue
            ldloc nilValue
            ldnull
            ceq
            define_label (fun lbl -> emit' {
                brfalse lbl
                ldstr ""
                stloc nilValue
                set_marker lbl
                ldloc nilValue
                callvirt_expr <@ "".ToLower() @>
                stloc nilValue
            })
            ldloc nilValue
            ldstr "1"
            string_equals
            define_labels_2 (fun lbl1 lbl2 -> emit' {
                brtrue lbl1
                ldloc nilValue
                ldstr "true"
                string_equals
                brtrue lbl1
                declare_variable (lazy declareLocal typ) (fun n -> emit' {
                    ldloca n
                    merge fdeser
                    ctor (typ.GetConstructor([| Nullable.GetUnderlyingType(typ) |]))
                    ldloc n
                })
                br lbl2
                set_marker lbl1
                ldnull
                set_marker lbl2
            })
        })
    }

let (|InlineContent|_|) (properties: Property list) =
    match properties with
    | [Individual({ Element = None; TypeMap = typeMap }) as prop]
    | [Array({ Element = None; ItemElement = None; ItemTypeMap = typeMap }) as prop] ->
        match typeMap.Layout with
        | Some(LayoutKind.Choice) -> None
        | _ -> Some(prop)
    | _ -> None

let systemTypeMap: Map<string, Lazy<(Emitter * Emitter)>> =
    Map.ofList
        [
            ("NodaTime.LocalDate",
                lazy
                    let typ = Noda.dateType.Value
                    let isoProp = Noda.datePattern.Value.GetProperty("Iso")
                    let formatMeth = Noda.datePattern.Value.GetMethod("Format", [| typ |])
                    let ser = emit' {
                        ldarg_0
                        call (isoProp.GetGetMethod())
                        ldarg_1
                        unbox typ
                        callvirt formatMeth
                        callvirt_expr <@ (null: XmlWriter).WriteValue("") @>
                    }
                    let parseMeth = Noda.datePattern.Value.GetMethod("Parse", [| typeof<string> |])
                    let valueOrThrowMeth = Noda.dateParseResult.Value.GetMethod("GetValueOrThrow", [||])
                    let deser = emit' {
                        call (isoProp.GetGetMethod())
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).ReadContentAsString() @>
                        callvirt parseMeth
                        callvirt valueOrThrowMeth
                    }
                    (ser, deser))
            ("NodaTime.LocalDateTime",
                lazy
                    let typ = Noda.dateTimeType.Value
                    let isoProp = Noda.dateTimePattern.Value.GetProperty("GeneralIso")
                    let formatMeth = Noda.dateTimePattern.Value.GetMethod("Format", [| typ |])
                    let ser = emit' {
                        ldarg_0
                        call (isoProp.GetGetMethod())
                        ldarg_1
                        unbox typ
                        callvirt formatMeth
                        callvirt_expr <@ (null: XmlWriter).WriteValue("") @>
                    }
                    let extendedIsoProp = Noda.dateTimePattern.Value.GetProperty("ExtendedIso").GetGetMethod()
                    let offsetExtendedIsoProp = Noda.dateTimePattern.Value.GetProperty("ExtendedIso").GetGetMethod()
                    let parseMethod = Noda.dateTimePattern.Value.GetMethod("Parse", [| typeof<string> |])
                    let successProp = Noda.dateTimeParseResult.Value.GetProperty("Success").GetGetMethod()
                    let valueProp = Noda.dateTimeParseResult.Value.GetProperty("Value").GetGetMethod()
                    let valueOrThrowMeth = Noda.offsetDateTimeParseResult.Value.GetMethod("GetValueOrThrow", [||])
                    let localDateTimeProp = Noda.offsetDateTimeType.Value.GetProperty("LocalDateTime").GetGetMethod()
                    let deser = emit' {
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).ReadContentAsString() @>
                        declare_variable (lazy declareLocalOf<string>) (fun value -> emit' {
                            stloc value
                            define_labels_2 (fun lbl1 lbl2 -> emit' {
                                call extendedIsoProp
                                ldloc value
                                callvirt parseMethod
                                declare_variable (lazy declareLocal Noda.dateTimeParseResult.Value) (fun result -> emit' {
                                    stloc result
                                    ldloc result
                                    callvirt successProp
                                    brfalse lbl1
                                    ldloc result
                                    callvirt valueProp
                                    br lbl2
                                    set_marker lbl1
                                    call offsetExtendedIsoProp
                                    ldloc value
                                    callvirt parseMethod
                                    callvirt valueOrThrowMeth
                                    callvirt localDateTimeProp
                                })
                                set_marker lbl2
                            })
                        })
                    }
                    (ser, deser))
        ]

let rec private createDeserializeContentMethodBody (typeMap: TypeMap) (properties: Property list) =
    emit' {
        define_label (fun returnLabel -> emit' {
            ldarg_2
            declare_variable (lazy declareLocalOf<int>) (fun depthVar -> emit' {
                stloc depthVar
                merge (
                    match properties with
                    | InlineContent(prop) ->
                        EmitDeserialization.emitPropertyDeserialization depthVar prop
                    | _ ->
                        emit' {
                            merge (
                                match typeMap.BaseType with
                                | Some(typeMap) ->
                                    emit' {
                                        ldarg_0
                                        ldarg_1
                                        ldloc depthVar
                                        ldarg_3
                                        call typeMap.Deserialization.Content
                                    }
                                | None -> id
                            )
                            define_label (fun startLabel -> emit' {
                                merge (match typeMap.Layout.Value with
                                       | LayoutKind.Sequence -> EmitDeserialization.emitSequenceDeserialization startLabel returnLabel depthVar properties
                                       | _ -> failwith "Not implemented")
                            })
                        }
                )
            })
            set_marker returnLabel
            ret
        })
    }

and createTypeSerializers isEncoded (typeMap: TypeMap) =
    let properties = getContentOfType typeMap |> getProperties (getTypeMap isEncoded)
    let directSubTypes = typeMap.Type |> findDirectSubTypes isEncoded

    // Emit serializers
    defineMethod typeMap.Serialization.Root
        (EmitSerialization.emitRootSerializerMethod isEncoded directSubTypes typeMap)

    defineMethod typeMap.Serialization.Content (emit' {
        merge (EmitSerialization.emitContentSerializerMethod isEncoded properties)
        ret
    })

    // Emit deserializers
    defineMethod typeMap.Deserialization.Root
        (EmitDeserialization.emitRootDeserializerMethod (match properties with InlineContent _ -> true | _ -> false) directSubTypes typeMap)

    defineMethod typeMap.Deserialization.Content
        (createDeserializeContentMethodBody typeMap properties)

    match properties with
    | [Individual { Element = None }] | [Array { Element = None; ItemElement = None }] ->
        ()
    | _ ->
        defineMethod typeMap.Deserialization.MatchType (emit' {
            merge (EmitDeserialization.emitMatchType (properties |> List.tryHead))
            ret
        })

and createChoiceTypeSerializers isEncoded (properties: Property list) (choiceMap: TypeMap) =
    let genSerialization () =
        let rec generate (conditionEnd: Label) (label: Label option) (properties: Property list) =
            match properties with
            | [] -> id
            | property::other ->
                let idField, tag = match property.Wrapper with Choice(_, idField, _, id, _) -> idField, id | _ -> failwith "never"
                let nextBlock nextLabel = emit' {
                    brfalse nextLabel
                    nop
                    merge (EmitSerialization.emitOptionalFieldSerialization property (EmitSerialization.emitPropertyContentSerialization (EmitSerialization.emitPropertyValue property) isEncoded property))
                    nop
                    br conditionEnd
                    merge (generate conditionEnd (Some nextLabel) other)
                }
                emit' {
                    merge (
                        match label with
                        | Some(label) ->
                            emit' {
                                set_marker label
                                nop
                            }
                        | None -> id
                    )
                    ldarg_1
                    castclass choiceMap.Type
                    ldfld idField
                    ldc_i4 tag
                    ceq
                    merge (if other.IsEmpty then (nextBlock conditionEnd) else (emit' { define_label nextBlock })) 
                }

        defineMethod choiceMap.Serialization.Root (emit' {
            define_label (fun conditionEnd -> emit' {
                merge (generate conditionEnd None properties)
                set_marker conditionEnd
                ret
            })
        })

    let genContentDeserialization () =
        defineMethod choiceMap.Deserialization.Content (emit' { ret })

    let genDeserialization () =
        let rec generate (depthVar: LocalBuilder) (markReturn: Label) (properties: Property list) =
            match properties with
            | [] -> id
            | property::other ->
                let mi = match property.Wrapper with Choice(_, _, _, _, mi) -> mi | _ -> failwith "never"
                emit' {
                    define_label (fun label -> emit' {
                        merge (
                            match property with
                            | Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                            | Individual { Element = None; TypeMap = typeMap }
                            | Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                            | Array { Element = None; ItemElement = None; ItemTypeMap = typeMap } ->
                                emit' {
                                    declare_variable (lazy (declareLocal property.Type)) (fun instance -> emit' {
                                        ldarg_0
                                        ldloc depthVar
                                        call_expr <@ (null: XmlReader).FindNextStartElement(0) @>
                                        pop
                                        ldarg_0
                                        call typeMap.Deserialization.MatchType
                                        brfalse label
                                        newobj (typeMap.Type.GetConstructor([| |]))
                                        stloc instance
                                        ldarg_0
                                        ldloc instance
                                        ldloc depthVar
                                        ldarg_1
                                        call typeMap.Deserialization.Content
                                        ldloc instance
                                    })
                                }
                            | Individual { Element = Some(name,_,_) }
                            | Array { Element = Some(name,_,_) }
                            | Array { Element = None; ItemElement = Some(name,_,_) } ->
                                emit' {
                                    ldarg_0
                                    ldloc depthVar
                                    call_expr <@ (null: XmlReader).FindNextStartElement(0) @>
                                    pop
                                    ldarg_0
                                    ldstr name.LocalName
                                    ldstr name.NamespaceName
                                    call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
                                    brfalse label
                                    merge (
                                        match property with
                                        | Individual propertyMap -> EmitDeserialization.emitIndividualPropertyDeserialization false propertyMap
                                        | Array arrayMap -> EmitDeserialization.emitArrayPropertyDeserialization false depthVar arrayMap
                                    )
                                }
                        )
                        call mi
                        br markReturn
                        set_marker label
                        nop
                        merge (generate depthVar markReturn other)
                    })
                }

        defineMethod choiceMap.Deserialization.Root (emit' {
            define_label (fun markReturn -> emit' {
                declare_variable (lazy declareLocalOf<int>) (fun depthVar ->
                    let names = properties |> List.map (fun p -> p.PropertyName)
                    let errorMessage = sprintf "Invalid message: expected one of %A, but `{0}` was found instead." names 
                    emit' {
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).Depth @>
                        stloc depthVar
                        merge (generate depthVar markReturn properties)
                        ldstr errorMessage
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).LocalName @>
                        call_expr <@ String.Format("", "") @>
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
                        ItemSimpleTypeName = getDefaultSystemTypeName elementType.FullName |> applyDataType attr
                        Wrapper = wrapper
                        GetMethod = getMethod
                        SetMethod = setMethod
                        HasValueMethod = hasValueMethod }
            else
                let propertyTypeMap = (if attr.UseXop then typeof<XopBinaryContent> else propertyType) |> tmf
                let element = if propertyTypeMap.Layout <> Some(LayoutKind.Choice) then element else None
                Individual { TypeMap = propertyTypeMap
                             SimpleTypeName = getDefaultSystemTypeName (propertyType.FullName) |> applyDataType attr
                             Element = element
                             Wrapper = wrapper
                             GetMethod = getMethod
                             SetMethod = setMethod
                             HasValueMethod = hasValueMethod })

and private typeMaps = ConcurrentDictionary<Type, TypeMap>()
and private uncompleteTypeMaps = ConcurrentDictionary<Type, TypeMap>()

and private addSystemTypeMap typ ser deser =
    let typeMap = TypeMap.Create(typ, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
    typeMap.IsComplete <- true
    typeMaps.TryAdd(typ, typeMap) |> ignore

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
    | Serializable(Some(typeAttribute)) ->
        typ |> addTypeMap (fun typeMap ->
            match typeAttribute.Layout with
            | LayoutKind.Choice -> typeMap |> createChoiceTypeSerializers isEncoded (getContentOfChoice typeMap |> getProperties (getTypeMap isEncoded))
            | _ -> typeMap |> createTypeSerializers isEncoded
            )
    | Serializable(None) ->
        let typName, isNullable = match Nullable.GetUnderlyingType(typ) with null -> typ.FullName, false | typ -> typ.FullName, true
        let emitSer, emitDeser = (systemTypeMap |> Map.find typName).Value
        if isNullable then
            let utyp = Nullable.GetUnderlyingType(typ)
            let nullSer = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true)
            defineMethod nullSer (EmitSerialization.emitSerializeNullable emitSer)
            let nullDeser = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |], true)
            defineMethod nullDeser (emit' {
                merge (EmitDeserialization.emitReadToNextWrapper typ (EmitDeserialization.emitDeserializeNullable typ (EmitDeserialization.emitDeserializeValue utyp emitDeser)))
                box typ
                ret
            })
            addSystemTypeMap typ nullSer nullDeser
        else
            let ser = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true)
            defineMethod ser emitSer
            let deser = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |], true)
            defineMethod deser (emit' {
                merge (EmitDeserialization.emitReadToNextWrapper typ (EmitDeserialization.emitDeserializeValue typ emitDeser))
                box typ
                ret
            })
            addSystemTypeMap typ ser deser
        typeMaps.[typ]

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
    open System.IO
    open System.Globalization

    let readToNextWrapper (r: XmlReader) f =
        let depth = r.Depth
        let name = r.LocalName
        let ns = r.NamespaceURI
        let v = f()
        r.ReadToNextElement(name, ns, depth, false)
        v

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

    let serializeDate (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(XmlConvert.ToString(unbox<DateTime> value, "yyyy-MM-dd"));

    let serializeDateTime (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(value);

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

    let dateFormats =
        [|
            "yyyy-MM-dd"
            "'+'yyyy-MM-dd"
            "'-'yyyy-MM-dd"
            "yyyy-MM-ddzzz"
            "'+'yyyy-MM-ddzzz"
            "'-'yyyy-MM-ddzzz"
            "yyyy-MM-dd'Z'"
            "'+'yyyy-MM-dd'Z'"
            "'-'yyyy-MM-dd'Z'"
        |]

    let deserializeDateValue value =
        if value |> String.IsNullOrEmpty then DateTime() else
        let date = DateTime.ParseExact(value, dateFormats, CultureInfo.InvariantCulture, DateTimeStyles.None);
        if value.[value.Length - 1] = 'Z' then date.ToLocalTime() else date

    let deserializeDateTimeValue value =
        if value |> String.IsNullOrEmpty then DateTime() else
        let dateTime = XmlConvert.ToDateTimeOffset(value).DateTime
        dateTime.AddTicks(-(dateTime.Ticks % TimeSpan.TicksPerSecond))

    let serializeNullableDefault (writer, value, context) = serializeNullable writer value context serializeDefault
    let serializeNullableBigInteger (writer, value, context) = serializeNullable writer value context serializeBigInteger
    let serializeNullableDate (writer, value, context) = serializeNullable writer value context serializeDate
    let serializeNullableDateTime (writer, value, context) = serializeNullable writer value context serializeDateTime

    let deserializeBoolean (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsBoolean)
    let deserializeDecimal (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsDecimal)
    let deserializeDouble (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsDouble)
    let deserializeInt32 (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsInt)
    let deserializeInt64 (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsLong)
    let deserializeBigInteger (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context (reader.ReadContentAsDecimal >> BigInteger))
    let deserializeDate (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context (fun () -> reader.ReadContentAsString() |> deserializeDateValue))
    let deserializeDateTime (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context (fun () -> reader.ReadContentAsString() |> deserializeDateTimeValue))

    let deserializeNullableBoolean (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeBoolean)
    let deserializeNullableDecimal (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeDecimal)
    let deserializeNullableDouble (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeDouble)
    let deserializeNullableInt32 (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeInt32)
    let deserializeNullableInt64 (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeInt64)
    let deserializeNullableBigInteger (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeBigInteger)
    let deserializeNullableDate (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeDate)
    let deserializeNullableDateTime (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeDateTime)
    let deserializeString (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeStringValue)

    let serializeBinaryContent (writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            let content = unbox<BinaryContent> value
            if context.IsMultipart then
                context.AddAttachment(content.ContentID, content, false)
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
            context.AddAttachment(content.ContentID, content, true)
            writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            writer.WriteEndElement()

    let deserializeBinaryContent (reader: XmlReader, context: SerializerContext) = readToNextWrapper reader (fun () ->
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            match reader.GetAttribute("href") with
            | null ->
                if reader.IsEmptyElement then BinaryContent.Create([| |]) else
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
    )

    let deserializeXopBinaryContent (reader: XmlReader, context: SerializerContext) = readToNextWrapper reader (fun () ->
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            if reader.IsEmptyElement then BinaryContent.Create([| |]) else
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
                | contentID -> reader.Read() |> ignore; context.GetAttachment(contentID)
            else BinaryContent.Create([| |])
    )

    let private addBinaryTypeMap typ ser deser =
        let typeMap = TypeMap.Create(typeof<BinaryContent>, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
        typeMap.IsComplete <- true
        typeMaps.TryAdd(typ, typeMap) |> ignore

    let mi e = match e with Call(_,mi,_) -> mi | _ -> failwith "do not use for that"

    let init () =
        addSystemTypeMap typeof<bool> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeBoolean(null, null) @>)
        addSystemTypeMap typeof<Nullable<bool>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableBoolean(null, null) @>)
        addSystemTypeMap typeof<decimal> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDecimal(null, null) @>)
        addSystemTypeMap typeof<Nullable<decimal>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDecimal(null, null) @>)
        addSystemTypeMap typeof<double> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDouble(null, null) @>)
        addSystemTypeMap typeof<Nullable<double>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDouble(null, null) @>)
        addSystemTypeMap typeof<int32> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt32(null, null) @>)
        addSystemTypeMap typeof<Nullable<int32>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt32(null, null) @>)
        addSystemTypeMap typeof<int64> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt64(null, null) @>)
        addSystemTypeMap typeof<Nullable<int64>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt64(null, null) @>)
        addSystemTypeMap typeof<BigInteger> (mi <@ serializeBigInteger(null, null, null) @>) (mi <@ deserializeBigInteger(null, null) @>)
        addSystemTypeMap typeof<Nullable<BigInteger>> (mi <@ serializeNullableBigInteger(null, null, null) @>) (mi <@ deserializeNullableBigInteger(null, null) @>)
        addSystemTypeMap typeof<DatePlaceholder> (mi <@ serializeDate(null, null, null) @>) (mi <@ deserializeDate(null, null) @>)
        addSystemTypeMap typeof<Nullable<DatePlaceholder>> (mi <@ serializeNullableDate(null, null, null) @>) (mi <@ deserializeNullableDate(null, null) @>)
        addSystemTypeMap typeof<DateTime> (mi <@ serializeDateTime(null, null, null) @>) (mi <@ deserializeDateTime(null, null) @>)
        addSystemTypeMap typeof<Nullable<DateTime>> (mi <@ serializeNullableDateTime(null, null, null) @>) (mi <@ deserializeNullableDateTime(null, null) @>)
        addSystemTypeMap typeof<string> (mi <@ serializeString(null, null, null) @>) (mi <@ deserializeString(null, null) @>)
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
