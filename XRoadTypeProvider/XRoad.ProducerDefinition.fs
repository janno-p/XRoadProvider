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

    let getOrCreateType (name: XName) =
        match typeCache.TryGetValue(name) with
        | true, typ -> typ
        | _ ->
            let typ = ProvidedTypeDefinition(name.LocalName, Some baseTy, IsErased=false)
            typ.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
            let ctor = ProvidedConstructor([], InvokeCode=(fun _ -> <@@ () @@>))
            typ.AddMember(ctor)
            let serializeMeth = ProvidedMethod("Serialize", [ProvidedParameter("writer", typeof<XmlWriter>)], typeof<System.Void>)
            serializeMeth.SetMethodAttrs(MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.VtableLayoutMask)
            typ.AddMember(serializeMeth)
            let namespaceTy = getOrCreateNamespace (name.Namespace)
            namespaceTy.AddMember(typ)
            typeCache.Add(name, typ)
            typ

    let getRuntimeType typeName =
        match mapPrimitiveType typeName with
        | Some tp -> tp
        | _ -> getOrCreateType typeName :> Type

    let rec buildType (providedTy: ProvidedTypeDefinition, typeInfo: SchemaType) =
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
                                else typ
                            | _ -> typeof<obj> // TODO: failwith "Not supported!"
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
                    | SequenceContent.Sequence(sequence) -> failwith "Not supported!")
            | _ -> ()
        match typeInfo with
        | SimpleType(SimpleTypeSpec.Restriction(spec)) ->
            ()
        | ComplexType(spec) ->
            let ctor = getConstructor(providedTy)
            if spec.IsAbstract then
                providedTy.SetAttributes(TypeAttributes.Abstract ||| TypeAttributes.Public ||| TypeAttributes.Class)
                ctor.SetConstructorAttrs(MethodAttributes.Family ||| MethodAttributes.RTSpecialName)
            match spec.Content with
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
                handleComplexTypeContentSpec(spec.Content)
            | ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                ()
            | ComplexTypeContent.Particle(spec) ->
                handleComplexTypeContentSpec(spec)
        serializeMeth.InvokeCode <-
            match serializeExpr |> List.ofSeq with
            | [] -> fun _ -> <@@ () @@>
            | exp::[] -> fun args -> exp(args)
            | exp::more -> fun args -> List.fold (fun exp e -> Expr.Sequential(exp, e args)) (exp args) more

    schema.TypeSchemas
    |> List.iter (fun typeSchema ->
        typeSchema.Types
        |> Seq.iter (fun kvp -> buildType(getOrCreateType(kvp.Key), kvp.Value)))

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
