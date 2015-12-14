module private XRoad.DynamicMethods

open FSharp.Quotations
open FSharp.Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open XRoad
open XRoad.Attributes

type DeserializerDelegate = delegate of XmlReader * SerializerContext -> obj
type SerializerDelegate = delegate of XmlWriter * obj * SerializerContext -> unit

type Serialization =
    { Root: MethodInfo
      Content: MethodInfo }

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
      Serialization: Serialization
      CanHaveNullAsValue: bool
      BaseType: TypeMap option }
    member this.Serialize(writer: XmlWriter, value: obj, context: SerializerContext) =
        this.SerializeDelegate.Value.Invoke(writer, value, context)
    member this.Deserialize(reader: XmlReader, context: SerializerContext) =
        this.DeserializeDelegate.Value.Invoke(reader, context)
    member this.FullName =
        match this.Namespace with
        | Some(ns) -> sprintf "{%s}:{%s}" ns this.Name
        | None -> this.Name
    static member Create(typ: Type, deserialization, serialization, baseType) =
        let attr = typ.GetCustomAttribute<XRoadTypeAttribute>() |> Option.ofObj
        let layout = attr |> Option.map (fun attr -> attr.Layout)
        { Type = typ
          Name = attr |> Option.fold (fun name attr -> match attr.Name with null | "" -> name | x -> x) typ.Name
          Namespace = attr |> Option.bind (fun attr -> match attr.Namespace with null | "" -> None | x -> Some(x))
          Layout = layout
          Deserialization = deserialization
          DeserializeDelegate = lazy (deserialization.Root.CreateDelegate(typeof<DeserializerDelegate>) |> unbox)
          Serialization = serialization
          SerializeDelegate = lazy (serialization.Root.CreateDelegate(typeof<SerializerDelegate>) |> unbox)
          CanHaveNullAsValue = (not (Nullable.GetUnderlyingType(typ) |> isNull)) || (typ.IsClass && layout <> Some(LayoutKind.Choice))
          BaseType = baseType }

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
    member this.GetMethod with get() = this |> function Individual x -> x.GetMethod | Array x -> x.GetMethod
    member this.PropertyName
        with get() =
            match this with
            | Individual { Element = Some(name,_) }
            | Array { Element = Some(name,_) }
            | Array { Element = None; ItemElement = Some(name,_) } -> Some(name)
            | _ -> None

type private XopBinaryContent() =
    inherit BinaryContent("", Data [| |])

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

module EmitSerialization =
    /// Check if values type matches expected type.
    let private emitValueTypeTest (il: ILGenerator) (expectedType: Type) =
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Call, !@ <@ (null: obj).GetType() @>)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: Type).FullName @>)
        il.Emit(OpCodes.Ldstr, expectedType.FullName)
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)

    /// Write type attribute according to TypeMap.
    let private emitTypeAttribute (il: ILGenerator) (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "type")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartAttribute("", "") @>)
        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, typeMap.Name)
        match typeMap.Namespace with
        | Some(ns) -> il.Emit(OpCodes.Ldstr, ns)
                      il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteQualifiedName("", "") @>)
        | None ->     il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteString("") @>)
        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndAttribute() @>)
        il.Emit(OpCodes.Nop)

    /// Emit type (and its base types) content serialization.
    let rec private emitContentSerialization (il: ILGenerator) (typeMap: TypeMap) =
        typeMap.BaseType |> Option.iter (emitContentSerialization il)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Ldarg_2)
        il.Emit(OpCodes.Call, typeMap.Serialization.Content)
        il.Emit(OpCodes.Nop)

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (il: ILGenerator) (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldstr, sprintf "Cannot serialize abstract type `%s`." typeMap.FullName)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)

    /// Emit whole contents of TypeMap serialization.
    let private emitBodySerialization (il: ILGenerator) addType (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then
            typeMap |> emitAbstractTypeException il
        else
            if addType then typeMap |> emitTypeAttribute il
            // TODO : Attributes
            typeMap |> emitContentSerialization il

    /// Emit serialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchySerialization il (markReturn: Label) (subTypes: TypeMap list) typeMap =
        match subTypes with
        | [] ->
            typeMap |> emitBodySerialization il false
        | subType::other ->
            let markNext = il.DefineLabel()

            // Check if type matches current TypeMap.
            subType.Type |> emitValueTypeTest il
            il.Emit(OpCodes.Brfalse_S, markNext)

            subType |> emitBodySerialization il true

            il.Emit(OpCodes.Br, markReturn)
            il.MarkLabel(markNext)
            il.Emit(OpCodes.Nop)
            typeMap |> emitTypeHierarchySerialization il markReturn other

    let emitNilAttribute (markReturn: Label) (il: ILGenerator) =
        let markNotNull = il.DefineLabel()
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse_S, markNotNull)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "nil")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Ldstr, "true")
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteAttributeString("", "", "") @>)
        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Br, markReturn)
        il.MarkLabel(markNotNull)
        il.Emit(OpCodes.Nop)

    /// Emit root type serialization logic for given TypeMap.
    let emitRootSerializerMethod (il: ILGenerator) (subTypes: TypeMap list) (typeMap: TypeMap) =
        let markReturn = il.DefineLabel()

        // When value is `null`, write `xsi:nil` attribute and return.
        il.Emit(OpCodes.Ldarg_1)
        il |> emitNilAttribute markReturn

        // Serialize value according to its type.
        typeMap |> emitTypeHierarchySerialization il markReturn subTypes

        // Return
        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)

    /// Provides value for array item at current index.
    let private emitArrayItemValue (il: ILGenerator) (array: LocalBuilder) (index: LocalBuilder) (typ: Type) =
        il.Emit(OpCodes.Ldloc, array)
        il.Emit(OpCodes.Ldloc, index)
        il.Emit(OpCodes.Ldelem, typ)
        if (typ.IsValueType) then
            il.Emit(OpCodes.Box, typ)

    /// Emit validation for not nullable types to have value specified.
    let private emitNotNullableCheck (il: ILGenerator) (name: string) emitValue property =
        match property with
        | Array _
        | Individual { TypeMap = { CanHaveNullAsValue = true } } ->
            let markSuccess = il.DefineLabel()

            // Check if value is null.
            emitValue property.Type
            il.Emit(OpCodes.Ldnull)
            il.Emit(OpCodes.Ceq)
            il.Emit(OpCodes.Brfalse_S, markSuccess)

            // Not nullable shouldn't have null as value, so throw exception.
            il.Emit(OpCodes.Ldstr, "Not nullable property `{0}` of type `{1}` has null value.")
            il.Emit(OpCodes.Ldstr, name)
            il.Emit(OpCodes.Ldstr, property.OwnerTypeMap.FullName)
            il.Emit(OpCodes.Call, !@ <@ String.Format("", "", "") @>)
            il.Emit(OpCodes.Newobj, !!@ <@ Exception("") @>)
            il.Emit(OpCodes.Throw)

            il.MarkLabel(markSuccess)
            il.Emit(OpCodes.Nop)
        | _ -> ()

    /// Emit single property content serialization.
    let rec private emitPropertyContentSerialization (il: ILGenerator) (emitValue: Type -> unit) (property: Property) =
        let markReturn = il.DefineLabel()

        // Write start element of the propery if its not merged with content.
        match property.Element with
        | Some(name, isNullable) ->
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldstr, name)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartElement("") @>)
            il.Emit(OpCodes.Nop)
            if not isNullable then property |> emitNotNullableCheck il name emitValue
        | None -> ()

        // Serialize property content value according to its TypeMap.
        match property with
        | Individual propertyMap ->
            il.Emit(OpCodes.Ldarg_0)
            emitValue propertyMap.TypeMap.Type
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Call, propertyMap.TypeMap.Serialization.Root)
            il.Emit(OpCodes.Nop)
        | Array arrayMap ->
            emitValue arrayMap.Type
            il |> emitNilAttribute markReturn
            let arr = il.DeclareLocal(arrayMap.Type)
            emitValue arrayMap.Type
            il.Emit(OpCodes.Stloc, arr)
            let i = il.DeclareLocal(typeof<int>)
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Stloc, i)
            let markLoopCondition = il.DefineLabel()
            il.Emit(OpCodes.Br_S, markLoopCondition)
            let markLoopStart = il.DefineLabel()
            il.MarkLabel(markLoopStart)
            il.Emit(OpCodes.Nop)
            let itemPropertyMap = Individual (arrayMap.GetItemPropertyMap())
            let itemEmitValue = emitArrayItemValue il arr i
            emitPropertyContentSerialization il itemEmitValue itemPropertyMap
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

        // Write end element if required.
        match property.Element with
        | Some(_) ->
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndElement() @>)
            il.Emit(OpCodes.Nop)
        | None -> ()

    /// Unbox property value into correct type.
    let private emitPropertyValue (il: ILGenerator) (property: Property) (typ: Type) =
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Castclass, property.OwnerTypeMap.Type)
        il.Emit(OpCodes.Callvirt, property.GetMethod)
        if typ.IsValueType then
            il.Emit(OpCodes.Box, typ)

    /// Emit IL which serializes each property value into corresponding xml fragment.
    let emitContentSerializerMethod (il: ILGenerator) (properties: Property list) =
        properties
        |> List.iter (fun property -> property |> emitPropertyContentSerialization il (emitPropertyValue il property))
        il.Emit(OpCodes.Ret)

module EmitDeserialization =
    /// Check if current element has `xsi:nil` attribute present.
    let emitNullCheck (markReturn: Label) (il: ILGenerator) =
        let nilValue = il.DeclareLocal(typeof<string>)

        // Get attribute value into local variable, in case of null empty string is used.
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

        // When attribute value is "true" or "1" return null.
        let markNull = il.DefineLabel()
        let markNotNull = il.DefineLabel()
        il.Emit(OpCodes.Ldloc, nilValue)
        il.Emit(OpCodes.Ldstr, "1")
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brtrue_S, markNull)
        il.Emit(OpCodes.Ldloc, nilValue)
        il.Emit(OpCodes.Ldstr, "true")
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brtrue_S, markNull)
        il.Emit(OpCodes.Br_S, markNotNull)

        // return null;
        il.MarkLabel(markNull)
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Br, markReturn)
        il.MarkLabel(markNotNull)
        il.Emit(OpCodes.Nop)

    /// Emit type (and its base types) content deserialization.
    let rec private emitContentDeserialization (il: ILGenerator) (instance: LocalBuilder) (typeMap: TypeMap) =
        //typeMap.BaseType |> Option.iter (emitContentDeserialization il instance)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, instance)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
        il.Emit(OpCodes.Nop)

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (il: ILGenerator) (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldstr, sprintf "Cannot deserialize abstract type `%s`." typeMap.FullName)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)

    /// Emit whole contents of TypeMap deserialization.
    let private emitBodyDeserialization (il: ILGenerator) (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then
            typeMap |> emitAbstractTypeException il
        else
            // Declare local variable to hold result.
            let instance = il.DeclareLocal(typeMap.Type)
            il.Emit(OpCodes.Newobj, typeMap.Type.GetConstructor([| |]))
            il.Emit(OpCodes.Stloc, instance)

            // TODO : Attributes

            typeMap |> emitContentDeserialization il instance

            // Prepare result for returning.
            il.Emit(OpCodes.Ldloc, instance)

    /// Check if value type matches expected type.
    let private emitValueTypeTest (il: ILGenerator) (typeName: LocalBuilder, typeNamespace: LocalBuilder) (markNext: Label) (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldloc, typeName)
        il.Emit(OpCodes.Ldstr, typeMap.Name)
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brfalse_S, markNext)
        il.Emit(OpCodes.Ldloc, typeNamespace)
        il.Emit(OpCodes.Ldstr, typeMap.Namespace |> Option.fold (fun _ x -> x) "")
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brfalse_S, markNext)

    /// Emit deserialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchyDeserialization il (markReturn: Label) (subTypes: TypeMap list) typeName typeMap =
        match subTypes with
        | [] ->
            typeMap |> emitBodyDeserialization il
        | subType::other ->
            let markNext = il.DefineLabel()

            // Check if type matches current TypeMap.
            subType |> emitValueTypeTest il typeName markNext

            // Deserialize content
            subType |> emitBodyDeserialization il

            il.Emit(OpCodes.Br, markReturn)
            il.MarkLabel(markNext)
            il.Emit(OpCodes.Nop)
            typeMap |> emitTypeHierarchyDeserialization il markReturn other typeName

    /// Reads type attribute value and stores name and namespace in variables.
    let private emitTypeAttributeRead (il: ILGenerator) (typeMap: TypeMap) =
        let typeName = il.DeclareLocal(typeof<string>)
        let typeNamespace = il.DeclareLocal(typeof<string>)

        let markParse = il.DefineLabel()
        let markDone = il.DefineLabel()
        let markDefaultName = il.DefineLabel()
        let markDefaultNamespace = il.DefineLabel()
        let markWithPrefix = il.DefineLabel()

        // Load empty string as default values.
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Stloc, typeNamespace)

        // When `xsi:type` is not present use default values.
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "type")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).GetAttribute("", "") @>)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Brtrue_S, markParse)
        il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Br, markDefaultName)

        // Parse `xsi:type` value into type name and namespace.
        il.MarkLabel(markParse)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Newarr, typeof<char>)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldc_I4, int32 ':')
        il.Emit(OpCodes.Stelem_I2)
        il.Emit(OpCodes.Ldc_I4_2)
        il.Emit(OpCodes.Callvirt, !@ <@ "".Split([| ':' |], 2) @>)
        il.Emit(OpCodes.Dup)

        // When default namespace is used (no prefix).
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Conv_I4)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse_S, markWithPrefix)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Br_S, markDefaultNamespace)

        // When prefix is present.
        il.MarkLabel(markWithPrefix)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeNamespace)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, typeNamespace)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LookupNamespace("") @>)
        il.Emit(OpCodes.Stloc, typeNamespace)
        il.Emit(OpCodes.Br_S, markDone)

        // Use TypeMap name as default value for typeName.
        il.MarkLabel(markDefaultName)
        il.Emit(OpCodes.Ldstr, typeMap.Name)
        il.Emit(OpCodes.Stloc, typeName)

        // Use default namespace when no prefix was found.
        il.MarkLabel(markDefaultNamespace)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LookupNamespace("") @>)
        il.Emit(OpCodes.Stloc, typeNamespace)

        il.MarkLabel(markDone)
        il.Emit(OpCodes.Nop)

        (typeName,typeNamespace)

    let emitRootDeserializerMethod (il: ILGenerator) (subTypes: TypeMap list) (typeMap: TypeMap) =
        let markReturn = il.DefineLabel()

        // When value nil attribute is present returns null.
        il |> emitNullCheck markReturn

        // Read type attribute value of current element.
        let typeName = typeMap |> emitTypeAttributeRead il

        // Serialize value according to its type.
        typeMap |> emitTypeHierarchyDeserialization il markReturn subTypes typeName

        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)

    let emitPropertyValueDeserialization (il: ILGenerator) (isContent: bool) (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(if isContent then OpCodes.Ldarg_3 else OpCodes.Ldarg_1)
        il.Emit(OpCodes.Call, typeMap.Deserialization.Root)
        match typeMap.Type.IsValueType with
        | true -> il.Emit(OpCodes.Unbox_Any, typeMap.Type)
        | _ -> il.Emit(OpCodes.Castclass, typeMap.Type)

    let emitIndividualPropertyDeserialization (il: ILGenerator) (propertyMap: PropertyMap) =
        let x = il.DeclareLocal(propertyMap.TypeMap.Type)
        emitPropertyValueDeserialization il true propertyMap.TypeMap
        il.Emit(OpCodes.Stloc, x)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Castclass, propertyMap.OwnerTypeMap.Type)
        il.Emit(OpCodes.Ldloc, x)
        il.Emit(OpCodes.Callvirt, propertyMap.SetMethod)

    let emitXmlReaderRead (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Read() @>)
        il.Emit(OpCodes.Nop)

    let emitXmlReaderReadOrExcept propertyName (il: ILGenerator) =
        let markSuccess = il.DefineLabel()
        il |> emitXmlReaderRead
        il.Emit(OpCodes.Brtrue_S, markSuccess)
        let errorMessage =
            match propertyName with
            | Some(name) -> sprintf "Invalid message: expected `%s`, but was end of file." name
            | None -> "Invalid message: unexpected end of file."
        il.Emit(OpCodes.Ldstr, errorMessage)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)
        il.MarkLabel(markSuccess)
        il.Emit(OpCodes.Nop)

    let emitArrayContentEndCheck (markArrayEnd: Label, varDepth: LocalBuilder) (il: ILGenerator) =
        let markSuccess = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
        il.Emit(OpCodes.Ldc_I4, int32 XmlNodeType.EndElement)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse_S, markSuccess)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Ldloc, varDepth)
        il.Emit(OpCodes.Clt)
        il.Emit(OpCodes.Brfalse_S, markSuccess)
        il.Emit(OpCodes.Br, markArrayEnd)
        il.MarkLabel(markSuccess)
        il.Emit(OpCodes.Nop)

    let emitXmlReaderDepthCheck (varDepth: LocalBuilder) (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Ldloc, varDepth)
        il.Emit(OpCodes.Ceq)

    let emitXmlReaderNodeTypeCheck (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ceq)

    let emitArrayItemDeserialization (arrayMap: ArrayMap, listInstance: LocalBuilder) (il: ILGenerator) =
        match arrayMap.ItemElement with
        | Some(name,_) ->
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
        | None -> ()

        il.Emit(OpCodes.Ldloc, listInstance)
        emitPropertyValueDeserialization il true arrayMap.ItemTypeMap
        il.Emit(OpCodes.Callvirt, listInstance.LocalType.GetMethod("Add", [| arrayMap.ItemTypeMap.Type |]))

    /// Emits array type deserialization logic.
    let emitArrayPropertyDeserialization (il: ILGenerator) (arrayMap: ArrayMap) =
        let varDepth = il.DeclareLocal(typeof<int>)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Stloc, varDepth)

        let markArrayEnd = il.DefineLabel()
        let markArrayNull = il.DefineLabel()

        if arrayMap.Element.IsSome then
            il |> emitNullCheck markArrayNull
            il.Emit(OpCodes.Ldloc, varDepth)
            il.Emit(OpCodes.Ldc_I4_1)
            il.Emit(OpCodes.Add)
            il.Emit(OpCodes.Stloc, varDepth)

        let listType = typedefof<System.Collections.Generic.List<_>>.MakeGenericType(arrayMap.ItemTypeMap.Type)
        let listInstance = il.DeclareLocal(listType)

        il.Emit(OpCodes.Newobj, listType.GetConstructor([| |]))
        il.Emit(OpCodes.Stloc, listInstance)

        let markSkipRead = il.DefineLabel()

        // Empty element has nothing to parse.
        if arrayMap.Element.IsSome then
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).IsEmptyElement @>)
            il.Emit(OpCodes.Brtrue, markArrayEnd)
        else il.Emit(OpCodes.Br_S, markSkipRead)

        let markLoopStart = il.DefineLabel()

        il.MarkLabel(markLoopStart)
        il |> emitXmlReaderReadOrExcept (arrayMap.ItemElement |> Option.map fst)
        il.MarkLabel(markSkipRead)

        il |> emitArrayContentEndCheck (markArrayEnd, varDepth)

        il |> emitXmlReaderDepthCheck varDepth
        il.Emit(OpCodes.Brfalse_S, markLoopStart)

        il |> emitXmlReaderNodeTypeCheck
        il.Emit(OpCodes.Brfalse_S, markLoopStart)

        il |> emitArrayItemDeserialization (arrayMap, listInstance)
        il.Emit(OpCodes.Br, markLoopStart)

        il.MarkLabel(markArrayEnd)

        let instance = il.DeclareLocal(arrayMap.Type)

        il.Emit(OpCodes.Ldloc,listInstance)
        il.Emit(OpCodes.Callvirt, listType.GetMethod("ToArray", [| |]))

        il.MarkLabel(markArrayNull)
        il.Emit(OpCodes.Stloc, instance)

        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Castclass, arrayMap.OwnerTypeMap.Type)
        il.Emit(OpCodes.Ldloc, instance)
        il.Emit(OpCodes.Callvirt, arrayMap.SetMethod)

let rec private createDeserializeContentMethodBody (il: ILGenerator) (typeMaps: TypeMap list) (properties: Property list) =
    let emitDeserialization (prop: Property) =
        match prop with
        | Individual propertyMap ->
            EmitDeserialization.emitIndividualPropertyDeserialization il propertyMap
        | Array arrayMap ->
            EmitDeserialization.emitArrayPropertyDeserialization il arrayMap

    let (|Content|_|) (properties: Property list) =
        match properties with
        | [Individual({ Element = None; TypeMap = typeMap }) as prop]
        | [Array({ Element = None; ItemElement = None; ItemTypeMap = typeMap }) as prop] ->
            match typeMap.Layout with
            | Some(LayoutKind.Choice) -> None
            | _ -> Some(prop)
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
            il.Emit(OpCodes.Ldarg_3)
            il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
            il.Emit(OpCodes.Nop))

        match typeMaps.Head.Layout.Value with
        | LayoutKind.Choice ->
            ()
        | LayoutKind.Sequence ->
            properties
            |> List.iteri (fun i prop ->
                let markLoopStart = il.DefineLabel()

                il.MarkLabel(markLoopStart)

                il |> EmitDeserialization.emitXmlReaderReadOrExcept prop.PropertyName

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
        let deserializeRootMethod = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |])
        let deserializeContentMethod = DynamicMethod(sprintf "%s_DeserializeContent" typ.FullName, null, [| typeof<XmlReader>; typeof<obj>; typeof<bool>; typeof<SerializerContext> |])
        let serializeRootMethod = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], typeAttribute.Layout = LayoutKind.Choice)
        let serializeContentMethod = DynamicMethod(sprintf "%s_SerializeContent" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |]) //, typeAttribute.Layout = LayoutKind.Choice)
        let matchTypeMethod = DynamicMethod(sprintf "%s_MatchType" typ.FullName, typeof<bool>, [| typeof<XmlReader> |])
        let deserialization = { Root = deserializeRootMethod; Content = deserializeContentMethod; MatchType = matchTypeMethod }
        let serialization: Serialization = { Root = serializeRootMethod; Content = serializeContentMethod }
        if typeMaps.TryAdd(typ, TypeMap.Create(typ, deserialization, serialization, findBaseType typ)) then
            match typeAttribute.Layout with
            | LayoutKind.Choice ->
                createChoiceTypeSerializers (serializeRootMethod.GetILGenerator()) (deserializeRootMethod.GetILGenerator()) (deserializeContentMethod.GetILGenerator()) (matchTypeMethod.GetILGenerator()) typ
            | _ ->
                let typeMap = getTypeMap typ
                let properties = typeMap |> getProperties
                let directSubTypes = typ |> findDirectSubTypes

                // Emit serializers
                EmitSerialization.emitRootSerializerMethod (serializeRootMethod.GetILGenerator()) directSubTypes typeMap
                EmitSerialization.emitContentSerializerMethod (serializeContentMethod.GetILGenerator()) properties

                // Emit deserializers
                EmitDeserialization.emitRootDeserializerMethod (deserializeRootMethod.GetILGenerator()) directSubTypes typeMap

                createDeserializeContentMethodBody (deserializeContentMethod.GetILGenerator()) (findBaseTypes typ) properties
                match properties with
                | [Individual { Element = None }]
                | [Array { Element = None; ItemElement = None }] -> ()
                | _ -> createMatchType (matchTypeMethod.GetILGenerator()) (properties |> List.tryHead)
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
                ilSer.Emit(OpCodes.Ldarg_2)
                ilSer.Emit(OpCodes.Call, (getTypeMap typ).Serialization.Root)
                ilSer.Emit(OpCodes.Nop)
            if attr.MergeContent then
                emitSerialization()
            else
                ilSer.Emit(OpCodes.Ldarg_0)
                ilSer.Emit(OpCodes.Ldstr, attr.Name)
                ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartElement("") @>)
                emitSerialization()
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
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
                    il.Emit(OpCodes.Ldloc, instance)
                else
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                    il.Emit(OpCodes.Ldstr, attr.Name)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                    il.Emit(OpCodes.Brfalse, label)
                    EmitDeserialization.emitPropertyValueDeserialization il false typeMap
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

and private getProperties (typeMap: TypeMap) : Property list =
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
                let itemTypeMap = getTypeMap (if attr.UseXop then typeof<XopBinaryContent> else p.PropertyType.GetElementType())
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
                let propertyTypeMap = getTypeMap (if attr.UseXop then typeof<XopBinaryContent> else p.PropertyType)
                let element = if propertyTypeMap.Layout <> Some(LayoutKind.Choice) then element else None
                Some(Individual { TypeMap = propertyTypeMap
                                  Element = element
                                  OwnerTypeMap = typeMap
                                  GetMethod = p.GetGetMethod()
                                  SetMethod = p.GetSetMethod() }))

and getTypeMap(typ) : TypeMap =
    match typeMaps.TryGetValue(typ) with
    | true, typeMap -> typeMap
    | false, _ -> createTypeMap typ

and findTypeMap (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> None
    | _ -> Some(getTypeMap typ)

and findBaseType (typ: Type) =
    if typ.BaseType |> isNull || typ.BaseType = typeof<obj> then None
    else match findTypeMap typ.BaseType with
         | None -> findBaseType typ.BaseType
         | typeMap -> typeMap

and findDirectSubTypes (typ: Type) : TypeMap list =
    typ.Assembly.GetTypes()
    |> List.ofArray
    |> List.filter (fun x -> x.BaseType = typ)
    |> List.choose findTypeMap

and findBaseTypes (typ: Type) =
    typ
    |> Seq.unfold (fun typ -> if typ = typeof<obj> then None else Some(findTypeMap typ, typ.BaseType))
    |> Seq.choose (id)
    |> Seq.toList

let createSystemTypeMap<'X> (writeMethods: MemberInfo list) (readMethods: MemberInfo list) =
    let createTypeMap isNullable =
        let typ = if isNullable then typedefof<Nullable<_>>.MakeGenericType(typeof<'X>) else typeof<'X>
        let serializerMethod =
            let meth = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |])
            let il = meth.GetILGenerator()
            let labelEnd =
                if isNullable || typ.IsClass then
                    let labelEnd = il.DefineLabel()
                    il.Emit(OpCodes.Ldarg_1)
                    il |> EmitSerialization.emitNilAttribute labelEnd
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
            let meth = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |])
            let il = meth.GetILGenerator()
            let labelRead = il.DefineLabel()
            let labelRet = il.DefineLabel()
            let labelCast = il.DefineLabel()
            if isNullable || typ.IsClass then
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
                il.Emit(OpCodes.Br, labelRet)
                il.MarkLabel(lbl3)
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
            il |> EmitDeserialization.emitXmlReaderRead
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
        let serialization: Serialization = { Root = serializerMethod; Content = null }
        typeMaps.TryAdd(typ, TypeMap.Create(typ, deserialization, serialization, None)) |> ignore
    if typeof<'X>.IsValueType then createTypeMap true
    createTypeMap false

let initBinaryContentSerialization useXop =
    let designType = if useXop then typeof<XopBinaryContent> else typeof<BinaryContent>
    let typ = typeof<BinaryContent>
    let serializeRootMethod =
        let meth = DynamicMethod(sprintf "%s_Serialize" designType.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |])
        let il = meth.GetILGenerator()
        if useXop then
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Call, !@ <@ BinaryContentHelper.SerializeXopBinaryContent(null, null, null) @>)
        else
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Call, !@ <@ BinaryContentHelper.SerializeBinaryContent(null, null, null) @>)
        il.Emit(OpCodes.Ret)
        meth
    let deserializeRootMethod =
        let meth = DynamicMethod(sprintf "%s_Deserialize" designType.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |])
        let il = meth.GetILGenerator()
        if useXop then
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Call, !@ <@ BinaryContentHelper.DeserializeXopBinaryContent(null, null) @>)
        else
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Call, !@ <@ BinaryContentHelper.DeserializeBinaryContent(null, null) @>)
        il.Emit(OpCodes.Ret)
        meth
    let deserialization = { Root = deserializeRootMethod; Content = null; MatchType = null }
    let serialization: Serialization = { Root = serializeRootMethod; Content = null }
    typeMaps.TryAdd(designType, TypeMap.Create(typ, deserialization, serialization, None)) |> ignore

do
    createSystemTypeMap<bool>
        []
        [!@ <@ (null: XmlReader).ReadContentAsBoolean() @>]
    createSystemTypeMap<decimal>
        []
        [!@ <@ (null: XmlReader).ReadContentAsDecimal() @>]
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
    initBinaryContentSerialization (true)
    initBinaryContentSerialization (false)
