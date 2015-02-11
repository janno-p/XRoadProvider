module internal XRoad.ProducerDefinition

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open System
open System.Xml
open System.Xml.Linq
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Wsdl.XsdSchema

let getConstructor (typ: Type) =
    typ.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance)
    |> Array.find (fun c -> c :? ProvidedConstructor) :?> ProvidedConstructor

let getProducerDefinition(uri, theAssembly, namespacePrefix) =
    let schema = resolveUri uri |> readSchema
    let typeCache = Dictionary<XName,ProvidedTypeDefinition>()
    let namespaceCache = Dictionary<XNamespace,ProvidedTypeDefinition>()

    let baseTy = typeof<obj>
    let serviceTypesTy = ProvidedTypeDefinition("ServiceTypes", Some baseTy, IsErased=false)

    let getOrCreateNamespace (name: XNamespace) =
        match namespaceCache.TryGetValue(name) with
        | true, typ -> typ
        | _ ->
            let producerName = 
                match Regex.Match(name.NamespaceName, @"^http://producers\.\w+\.xtee\.riik\.ee/producer/(\w+)$") with
                | m when m.Success -> m.Groups.[1].Value
                | _ -> failwithf "TODO: Implement normal namespace handling for tns: %A" name
            let typ = ProvidedTypeDefinition(producerName, Some baseTy, IsErased=false)
            serviceTypesTy.AddMember(typ)
            namespaceCache.Add(name, typ)
            typ

    let createWriteTypeAttributeMethod (typeName: XName) =
        let meth = ProvidedMethod("WriteTypeAttribute", [ProvidedParameter("writer", typeof<XmlWriter>)], typeof<Void>)
        meth.InvokeCode <- fun args ->
            let nm, ns = typeName.LocalName, typeName.NamespaceName
            <@@ (%%args.[1]: XmlWriter).WriteStartAttribute("type", XmlNamespace.Xsi)
                (%%args.[1]: XmlWriter).WriteQualifiedName(nm, ns)
                (%%args.[1]: XmlWriter).WriteEndAttribute() @@>
        meth

    let createType name =
        let typ = ProvidedTypeDefinition(name, Some baseTy, IsErased=false)
        typ.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
        let ctor = ProvidedConstructor([], InvokeCode=(fun _ -> <@@ () @@>))
        typ.AddMember(ctor)
        let parameters = [ ProvidedParameter("writer", typeof<XmlWriter>)
                           ProvidedParameter("needsType", typeof<bool>) ]
        let serializeMeth = ProvidedMethod("Serialize", parameters, typeof<System.Void>)
        serializeMeth.SetMethodAttrs(MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.VtableLayoutMask)
        typ.AddMember(serializeMeth)
        typ

    let getOrCreateType (name: XName) =
        match typeCache.TryGetValue(name) with
        | true, typ -> typ
        | _ ->
            let typ = createType name.LocalName
            let namespaceTy = getOrCreateNamespace (name.Namespace)
            namespaceTy.AddMember(typ)
            typeCache.Add(name, typ)
            typ

    let getRuntimeType typeName =
        match mapPrimitiveType typeName with
        | Some tp -> tp
        | _ -> getOrCreateType typeName :> Type

    let (|NullableType|_|) (typ: Type) =
        match Nullable.GetUnderlyingType(typ) with
        | null -> None
        | typ -> Some(typ)

    let (|SoapEncArray|_|) (elementType: RefOrType) =
        let getArrayType arrayType =
            match arrayType with
            | Some(typeName, rank) ->
                [1..rank] |> List.fold (fun (aggTyp: Type) _ -> aggTyp.MakeArrayType()) (getRuntimeType(typeName))
            | _ -> failwith "Array underlying type specification is missing."

        match elementType with
        | RefOrType.Type(SchemaType.ComplexType(spec)) ->
            match spec.Content with
            | ComplexTypeContent.ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                match spec.Base.LocalName, spec.Base.NamespaceName with
                | "Array", XmlNamespace.SoapEncoding ->
                    match spec.Content.Attributes with
                    | [ arrayType ] when arrayType.Name = Some("arrayType") ->
                        Some(getArrayType(arrayType.ArrayType))
                    | [ arrayType ] when arrayType.RefOrType = RefOrType.Ref(XName.Get("arrayType", XmlNamespace.SoapEncoding)) ->
                        Some(getArrayType(arrayType.ArrayType))
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None

    let rec mkSerializeExpr (writer: Expr, fieldExpr: Expr, typ: Type) =
        match typ with
        | :? ProvidedTypeDefinition ->
            let getType = typeof<obj>.GetMethod("GetType")
            let getTypeExpr = Expr.Call(Expr.Coerce(fieldExpr, typeof<obj>), getType, [])
            let typeTest = typeof<Type>.GetMethod("IsAssignableFrom")
            let typeTestExpr = Expr.Call(getTypeExpr, typeTest, [Expr.Value(typ)])
            Expr.Call(fieldExpr, typ.GetMethod("Serialize"), [writer; typeTestExpr])
        | NullableType(typ) when typ = typeof<int32> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: Nullable<int32>).Value) @@>
        | typ when typ = typeof<int32> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: int32)) @@>
        | NullableType(typ) when typ = typeof<decimal> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: Nullable<decimal>).Value) @@>
        | typ when typ = typeof<decimal> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: decimal)) @@>
        | NullableType(typ) when typ = typeof<int64> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: Nullable<int64>).Value) @@>
        | typ when typ = typeof<int64> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: int64)) @@>
        | NullableType(typ) when typ = typeof<DateTime> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: Nullable<DateTime>).Value) @@>
        | typ when typ = typeof<DateTime> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: DateTime)) @@>
        | NullableType(typ) when typ = typeof<bool> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: Nullable<bool>).Value) @@>
        | typ when typ = typeof<bool> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: bool)) @@>
        | typ when typ = typeof<string> ->
            <@@ (%%writer: XmlWriter).WriteValue((%%fieldExpr: string)) @@>
        | typ when typ = typeof<byte[]> ->
            <@@ (%%writer: XmlWriter).WriteBase64((%%fieldExpr: byte[]), 0, (%%fieldExpr: byte[]).Length) @@>
        | typ when typ.IsArray ->
            // TODO: item tag name
            let coerseExpr = Expr.Coerce(fieldExpr, typeof<Array>)
            let v = Var("i", typeof<int>)
            let vExpr = Expr.Var(v)
            let elTyp = typ.GetElementType()
            let getValExpr = <@@ (%%coerseExpr: Array).GetValue(%%vExpr: int) @@>
            let itemExpr = mkSerializeExpr(writer, Expr.Coerce(getValExpr, elTyp), elTyp)
            Expr.ForIntegerRangeLoop(
                v,
                <@@ 0 @@>,
                <@@ ((%%coerseExpr: Array).Length - 1) @@>,
                Expr.Sequential(
                    <@@ (%%writer: XmlWriter).WriteStartElement("item") @@>,
                    Expr.Sequential(itemExpr, <@@ (%%writer: XmlWriter).WriteEndElement() @@>)))
        | typ -> failwithf "Serialization method for %A is not defined" typ.FullName

    let rec buildType (providedTy: ProvidedTypeDefinition, typeInfo: SchemaType, typeName: XName option) =
        let serializeMeth = providedTy.GetMethod("Serialize") :?> ProvidedMethod
        let serializeExpr = List<(Expr list -> Expr)>()
        let handleComplexTypeContentSpec (spec: ComplexTypeContentSpec) =
            if not(List.isEmpty spec.Attributes) then
                failwithf "TODO: handle complex type content attributes for type %A" providedTy.Name
            match spec.Content with
            | Some(ComplexTypeParticle.All(particle)) -> ()
            | Some(ComplexTypeParticle.Sequence(sequence)) ->
                if sequence.MinOccurs <> 1u || sequence.MaxOccurs <> 1u then
                    failwith "Not supported!"
                sequence.Content |> List.iter (fun item ->
                    match item with
                    | SequenceContent.Element(element) ->
                        let elementType =
                            match element.Type with
                            | RefOrType.Name(xname) ->
                                let typ = getRuntimeType(xname)
                                if element.MaxOccurs > 1u then typ.MakeArrayType()
                                elif element.IsNillable && typ.IsValueType then typedefof<Nullable<_>>.MakeGenericType(typ)
                                else typ
                            | SoapEncArray arrType -> arrType
                            | RefOrType.Type(typeSpec) ->
                                let subTy = createType(element.Name + "Type")
                                providedTy.AddMember(subTy)
                                buildType(subTy, typeSpec, None)
                                if element.MaxOccurs > 1u then subTy.MakeArrayType()
                                else subTy :> Type
                            | _ -> failwithf "Not supported: %A!" element.Type
                        let setField =
                            match element.MinOccurs with
                            | 0u -> Some(ProvidedField("s__" + element.Name, typeof<bool>))
                            | _ -> None
                        let backingField = ProvidedField("b__" + element.Name, elementType)
                        providedTy.AddMembers(setField |> Option.fold (fun d f -> f::d) [backingField])
                        let prop = ProvidedProperty(element.Name, elementType)
                        prop.GetterCode <- fun args -> Expr.FieldGet(args.[0], backingField)
                        prop.SetterCode <- fun args ->
                            let valueExpr = Expr.FieldSet(args.[0], backingField, args.[1])
                            match setField with
                            | Some(f) -> Expr.Sequential(valueExpr, Expr.FieldSet(args.[0], f, Expr.Value(true)))
                            | _ -> valueExpr
                        providedTy.AddMember(prop)
                        serializeExpr.Add(fun args ->
                            let objEquals = typeof<obj>.GetMethod("ReferenceEquals")
                            let fieldExpr = Expr.FieldGet(args.[0], backingField)
                            let fVal = Expr.Coerce(fieldExpr, typeof<obj>)
                            let fieldValue = Expr.Call(objEquals, [fVal; Expr.Value(null)])
                            let testNullExpr =
                                let serExpr = mkSerializeExpr(args.[1], fieldExpr, elementType)
                                Expr.IfThenElse(fieldValue,
                                                (if element.IsNillable then <@@ (%%args.[1]: XmlWriter).WriteAttributeString("nil", XmlNamespace.Xsi, "true") @@> else Expr.Value(())),
                                                serExpr)
                            let expr =
                                let nm = element.Name
                                <@@ (%%args.[1]: XmlWriter).WriteStartElement(nm)
                                    %%testNullExpr
                                    (%%args.[1]: XmlWriter).WriteEndElement() @@>
                            match setField with
                            | Some(field) -> Expr.IfThenElse(Expr.FieldGet(args.[0], field), expr, Expr.Value(()))
                            | _ -> expr)
                    | SequenceContent.Sequence(sequence) -> failwith "Not supported!")
            | _ -> ()
        match typeInfo with
        | SimpleType(SimpleTypeSpec.Restriction(spec)) ->
            ()
        | ComplexType(ctspec) ->
            let ctor = getConstructor(providedTy)
            if ctspec.IsAbstract then
                providedTy.SetAttributes(TypeAttributes.Abstract ||| TypeAttributes.Public ||| TypeAttributes.Class)
                ctor.SetConstructorAttrs(MethodAttributes.Family ||| MethodAttributes.RTSpecialName)
            match ctspec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                ()
            | SimpleContent(SimpleContentSpec.Restriction(spec)) ->
                ()
            | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                let baseTy = getOrCreateType(spec.Base)
                providedTy.SetBaseType(baseTy)
                let baseCtor = getConstructor(baseTy)
                ctor.BaseConstructorCall <- (fun args -> baseCtor :> ConstructorInfo, args)
                serializeMeth.SetMethodAttrs(MethodAttributes.Public ||| MethodAttributes.Virtual)
                let baseSerialize = baseTy.GetMethod("Serialize") :?> ProvidedMethod
                serializeExpr.Add(fun args -> Expr.Call(args.Head, baseSerialize, args.Tail))
                match typeName with
                | Some(typeName) ->
                    if not(ctspec.IsAbstract) then
                        let attrMeth = createWriteTypeAttributeMethod(typeName)
                        attrMeth.SetMethodAttrs(MethodAttributes.Family ||| MethodAttributes.Virtual)
                        providedTy.AddMember(attrMeth)
                | _ -> ()
                handleComplexTypeContentSpec(spec.Content)
            | ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                ()
            | ComplexTypeContent.Particle(spec) ->
                let attrMeth =
                    match typeName with
                    | Some(typeName) ->
                        let attrMeth = createWriteTypeAttributeMethod(typeName)
                        attrMeth.SetMethodAttrs(MethodAttributes.Family ||| MethodAttributes.Virtual ||| MethodAttributes.VtableLayoutMask)
                        if ctspec.IsAbstract then attrMeth.AddMethodAttrs(MethodAttributes.Abstract)
                        providedTy.AddMember(attrMeth)
                        Some(attrMeth)
                    | _ -> None
                match attrMeth with
                | Some(attrMeth) ->
                    serializeExpr.Add(fun args ->
                        if ctspec.IsAbstract then Expr.Call(args.[0], attrMeth, [args.[1]])
                        else Expr.IfThenElse(args.[2], Expr.Call(args.[0], attrMeth, [args.[1]]), Expr.Value(())))
                | _ -> ()
                handleComplexTypeContentSpec(spec)
        serializeMeth.InvokeCode <-
            match serializeExpr |> List.ofSeq with
            | [] -> fun _ -> <@@ () @@>
            | exp::[] -> fun args -> exp(args)
            | more -> fun args -> List.foldBack (fun e exp -> Expr.Sequential(e args, exp)) more (Expr.Value(()))

    schema.TypeSchemas
    |> List.iter (fun typeSchema ->
        typeSchema.Types
        |> Seq.iter (fun kvp -> buildType(getOrCreateType(kvp.Key), kvp.Value, Some(kvp.Key))))

    let serviceTypes =
        schema.Services |> List.map (fun service ->
            let serviceTy = ProvidedTypeDefinition(service.Name, Some baseTy, IsErased=false)
            service.Ports |> List.map (fun port ->
                let portTy = ProvidedTypeDefinition(port.Name, Some baseTy, IsErased=false)

                let addressField = ProvidedField("b__Address", typeof<string>)
                portTy.AddMember(addressField)

                let addressProperty = ProvidedProperty("Address", typeof<string>)
                addressProperty.GetterCode <- fun args -> Expr.FieldGet(args.[0], addressField)
                addressProperty.SetterCode <- fun args -> Expr.FieldSet(args.[0], addressField, args.[1])
                portTy.AddMember(addressProperty)

                let producerField = ProvidedField("b__Producer", typeof<string>)
                portTy.AddMember(producerField)

                let producerProperty = ProvidedProperty("Producer", typeof<string>)
                producerProperty.GetterCode <- fun args -> Expr.FieldGet(args.[0], producerField)
                producerProperty.SetterCode <- fun args -> Expr.FieldSet(args.[0], producerField, args.[1])
                portTy.AddMember(producerProperty)

                let addressValue = port.Address
                let producerValue = port.Producer

                let ctor = ProvidedConstructor([])
                ctor.InvokeCode <- fun args -> Expr.Sequential(Expr.FieldSet(args.[0], addressField, Expr.Value(addressValue)),
                                                               Expr.FieldSet(args.[0], producerField, Expr.Value(producerValue)))
                portTy.AddMember(ctor)

                match port.Documentation.TryGetValue("et") with
                | true, docString -> portTy.AddXmlDoc(docString)
                | _ -> ()

                port.Operations |> List.map (fun op -> ()) |> ignore // TODO!!

                portTy)
            |> serviceTy.AddMembers
            serviceTy)

    serviceTypesTy::serviceTypes
