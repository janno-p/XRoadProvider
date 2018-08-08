module internal XRoad.ProducerDefinition

open System
open System.Reflection
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open XRoad
open CodeDom
open Wsdl

/// Functions and types to handle building methods for services and operation bindings.
module ServiceBuilder =
    open System.Xml
    open TypeSchema
    open XRoad.Serialization.Attributes
    open System.Text

    /// Creates return type for the operation.
    /// To support returning multiple output parameters, they are wrapped into tuples accordingly:
    /// Single parameter responses return that single parameter.
    /// Multiple parameter responses are wrapped into tuples, since C# provides tuples upto 8 arguments,
    /// some composition is required when more output parameters are present.
//    let private makeReturnType isMultipart (types: (string * RuntimeType) list) =
//        let rec getReturnTypeTuple (tuple: (string * RuntimeType) list, types) =
//            match types with
//            | [] -> let typ = CodeTypeReference("System.Tuple", tuple |> List.map (fun (_,typ) -> typ.AsCodeTypeReference()) |> Array.ofList)
//                    (typ, Expr.instOf typ (tuple |> List.map (fun (varName,_) -> !+ varName)))
//            | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
//            | x::xs -> let inner = getReturnTypeTuple([x], xs)
//                       let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_,typ) -> typ.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
//                       (typ, Expr.instOf typ ((tuple |> List.map (fun (varName,_) -> !+ varName)) @ [snd inner]))
//        let types =
//            if isMultipart
//            then ("reader.Context.Attachments", PrimitiveType(typeof<IDictionary<string,Stream>>))::types
//            else types
//        match types with
//        | [] -> (CodeTypeReference(typeof<Void>), Expr.empty)
//        | [(varName, typ)] -> (typ.AsCodeTypeReference(), !+ varName)
//        | many -> getReturnTypeTuple([], many)

    let instQN (nm: string) (ns: string) =
        Expr.NewObject(typeof<XmlQualifiedName>.GetConstructor([| typeof<string>; typeof<string> |]), [ Expr.Value(nm); Expr.Value(ns) ])

//    let buildOperationOutput (context: TypeBuilderContext) (operation: ServicePortMethod) protocol (_: List<CodeTypeMember>) m =
//        let x () =
//            let resultClass = Cls.create(sprintf "%sOutput" operation.Name) |> Cls.setAttr (TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed) |> Cls.describe Attributes.xrdRoot
//            m |> Meth.addStmt (Stmt.declVarWith<XRoad.XRoadResponseOptions> "@__respOpt" (Expr.inst<XRoad.XRoadResponseOptions> [!^ operation.OutputParameters.IsEncoded; !^ operation.OutputParameters.IsMultipart; Expr.typeRefOf<XRoad.XRoadProtocol> @=> protocol.ToString(); Expr.typeOf (CodeTypeReference(resultClass.Name)) ])) |> ignore
//        let addParameter (parameter: Parameter) nm ns =
//            let runtimeType = context.GetRuntimeType(match parameter.Type with Some(typeName) -> SchemaType(typeName) | None -> SchemaElement(parameter.Name))
//            let prop = resultClass |> addProperty (parameter.Name.LocalName, runtimeType, false)
//            let attr = Attributes.xrdElement (nm, ns, false)
//            prop |> Prop.describe (match nm, ns with None, None -> attr |> Attr.addNamedArg "MergeContent" (!^ true) | _ -> attr) |> ignore
//        match parameters with
//        | [] ->
//            m |> Meth.addStmt (Stmt.ret Expr.empty)
//        | [(name, typ)] ->
//            m |> Meth.returnsOf (typ.AsCodeTypeReference())
//              |> Meth.addStmt (Stmt.ret ((Expr.cast (CodeTypeReference(resultClass.Name)) ((!+ "@__r") @=> "Body")) @=> name.LocalName))
//        | _ ->
//            m |> Meth.returns<obj>
//              |> Meth.addStmt (Stmt.ret Expr.nil)
//        | _ -> m

    /// Build content for each individual service call method.
    let build (context: TypeBuilderContext) tns (operation: ServicePortMethod): MemberInfo list =
        let parameters = ResizeArray<ProvidedParameter>()

        match context.MessageProtocol with
        | Version20(_) ->
            parameters.Add(ProvidedParameter("header", typeof<XRoadRpcHeader>))
        | Version30(_) | Version31Ee(_) | Version31Eu(_) ->
            parameters.Add(ProvidedParameter("header", typeof<XRoadDocHeader>))
        | Version40(_) ->
            parameters.Add(ProvidedParameter("header", typeof<XRoadHeader>))

        let additionalMembers = ResizeArray<MemberInfo>()
        let protocol = context.MessageProtocol.EnumValue
        let attributes = ResizeArray<CustomAttributeData>()
        let paramDoc = System.Collections.Generic.Dictionary<string,string>()

        let addDocLiteralWrappedParameters (spec: ElementSpec) =
            let choiceNameGen = TypeBuilder.nameGenerator (sprintf "%sChoiceArg" operation.Name)
            let argNameGen = TypeBuilder.nameGenerator "choiceArg"
            match context.DereferenceElementSpec(spec) |> snd |> context.GetSchemaTypeDefinition with
            | EmptyDefinition -> ()
            | ComplexDefinition({ IsAbstract = false; Content = Particle({ Content = Some(ComplexTypeParticle.Sequence({ Content = content; MinOccurs = 1u; MaxOccurs = 1u })) }) }) ->
                content
                |> List.iter (fun value ->
                    match value with
                    | Element(elementSpec) ->
                        let dspec, schemaType = context.DereferenceElementSpec(elementSpec)
                        let name = dspec.Name |> Option.get
                        let runtimeType =
                            match schemaType with
                            | Definition(definition) ->
                                let subTy = ProvidedTypeDefinition(sprintf "%s_%sType" operation.Name name, Some typeof<obj>, isErased = false)
                                subTy.AddCustomAttribute(Attributes.xrdAnonymousType LayoutKind.Sequence)
                                let ns = context.GetOrCreateNamespace(tns)
                                ns.AddMember(subTy)
                                let runtimeType = ProvidedType(subTy, providedTypeFullName ns.Name subTy.Name)
                                TypeBuilder.build context runtimeType definition
                                runtimeType
                            | Name(typeName) -> context.GetRuntimeType(SchemaType(typeName))
                        let p =
                            let isOptional = dspec.MinOccurs = 0u
                            let par = ProvidedParameter(name, runtimeType |> runtimeTypeToSystemType isOptional)
                            par.AddCustomAttribute(Attributes.xrdElement None None None false false dspec.ExpectedContentTypes.IsSome)
                            if isOptional then par.AddCustomAttribute(Attributes.Optional)
                            par
                        parameters.Add(p)
                    | Choice(particleSpec) ->
                        let def, addedTypes = TypeBuilder.collectChoiceProperties choiceNameGen context particleSpec
                        let p =
                            let argName = argNameGen()
                            let par = ProvidedParameter(argName, def.Type |> runtimeTypeToSystemType def.IsOptional)
                            par.AddCustomAttribute(Attributes.xrdElement None None None def.IsNillable false false)
                            if def.Documentation.IsSome then paramDoc.Add(argName, def.Documentation.Value)
                            par
                        parameters.Add(p)
                        additionalMembers.AddRange(addedTypes |> Seq.cast<_>)
                    | _ -> failwithf "%A" value)
            | _ -> failwithf "Input wrapper element must be defined as complex type that is a sequence of elements (erroneous XML Schema entity `%s`)." (spec.Name |> MyOption.defaultValue "<unknown>")

        match operation.InputParameters with
//        | DocEncoded(encodingNamespace, wrapper) ->
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) (Some(encodingNamespace.NamespaceName)))
//        | DocLiteralBody(content) ->
//            addParameter content.Parameters.Head None None
        | DocLiteralWrapped(name, content) ->
            attributes.Add(Attributes.xrdRequest name.LocalName name.NamespaceName false content.HasMultipartContent)
            name |> context.GetElementSpec |> addDocLiteralWrappedParameters
//        | DocLiteral(wrapper) ->
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) (Some(p.Name.NamespaceName)))
//        | RpcEncoded(accessor, wrapper) ->
//            m |> Meth.addStmt (Stmt.assign (!+ "@__reqOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
//        | RpcLiteral(accessor, { Parameters = [{ Type = Some(_) } as p] }) ->
//            addParameter p (Some(accessor.LocalName)) (Some(accessor.NamespaceName))
//        | RpcLiteral(accessor, wrapper) ->
//            m |> Meth.addStmt (Stmt.assign (!+ "@__reqOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
        | _ -> ()

        let mutable returnType = None
        let mutable invokeCode = None

        // buildOperationOutput context operation protocol result |> ignore
        match operation.OutputParameters with
        | DocLiteralWrapped(name, content) ->
            let elementType = TypeBuilder.buildResponseElementType context name
            let resultClass =
                match elementType with
                | CollectionType(_, itemName, _) ->
                    let elementSpec = name |> context.GetElementSpec
                    let resultClass =
                        let t = ProvidedTypeDefinition(sprintf "%sResult" operation.Name, Some typeof<obj>, isErased = false)
                        t.SetAttributes(TypeAttributes.Class ||| TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed)
                        t.AddCustomAttribute(Attributes.xrdAnonymousType LayoutKind.Sequence)
                        t
                    let responseProp = resultClass |> addProperty("response", elementType, false)
                    responseProp.AddCustomAttribute(Attributes.xrdElement None None None false true elementSpec.ExpectedContentTypes.IsSome)
                    responseProp.AddCustomAttribute(Attributes.xrdCollection None (Some(itemName)) None false false)
                    Some(resultClass, responseProp)
                | _ -> None
            returnType <- Some (elementType |> runtimeTypeToSystemType false)
            match resultClass with
            | Some(cls, responseProp) ->
                attributes.Add(Attributes.xrdResponse name.LocalName name.NamespaceName false content.HasMultipartContent (Some cls))
                invokeCode <-
                    (fun args ->
                        let varResult = Var("__result", cls)
                        Expr.Let(
                            varResult,
                            Expr.Coerce(Expr.Value(null)(*Expr.Call(typeof<XRoad.XRoadUtil>.GetMethod("MakeServiceCall"), [ (* args.[0]; Expr.Value(operation.Name); args.[1]; new[] { args.[2..] } *) ])*), cls),
                            Expr.PropertyGet(Expr.Var(varResult), responseProp)
                        )
                    ) |> Some
                additionalMembers.Add(cls)
            | None ->
                attributes.Add(Attributes.xrdResponse name.LocalName name.NamespaceName false content.HasMultipartContent None)
                invokeCode <-
                    (fun args ->
                        Expr.Coerce(Expr.Value(null)(*Expr.Call(typeof<XRoad.XRoadUtil>.GetMethod("MakeServiceCall"), [ (* args.[0]; Expr.Value(operation.Name); args.[1]; new[] { args.[2..] } *) ])*), returnType.Value)
                    ) |> Some
//        | DocEncoded(encodingNamespace, wrapper) ->
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) (Some(encodingNamespace.NamespaceName)))
//            m
//        | DocLiteralBody(content) ->
//            addParameter content.Parameters.Head None None
//            m
//        | DocLiteral(wrapper) ->
//            wrapper.Parameters
//            |> List.map (fun p ->
//                addParameter p (Some(p.Name.LocalName)) (Some(p.Name.NamespaceName))
//                (p.Name, context.GetRuntimeType(SchemaElement(p.Name))))
//            m
//        | RpcEncoded(accessor, wrapper) ->
//            m |> Meth.addStmt (Stmt.assign (!+ "@__respOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
//            m
//        | RpcLiteral(accessor, { Parameters = [{ Type = Some(_) } as p] }) ->
//            addParameter p (Some(accessor.LocalName)) (Some(accessor.NamespaceName))
//            m
//        | RpcLiteral(accessor, wrapper) ->
//            m |> Meth.addStmt (Stmt.assign (!+ "@__respOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
//            m
        | _ -> ()

        assert returnType.IsSome
        assert invokeCode.IsSome

        let doc =
            let db = StringBuilder()
            operation.Documentation |> Option.iter (db.AppendLine >> ignore)
            paramDoc |> Seq.iter (fun kvp -> db.AppendLine(sprintf "<param>%s</param> %s" kvp.Key kvp.Value) |> ignore)
            db.ToString()

        let meth =
            let m = ProvidedMethod(operation.Name, parameters |> Seq.toList, returnType.Value, invokeCode = invokeCode.Value)
            m.AddCustomAttribute(Attributes.xrdOperation operation.Name operation.Version protocol context.MessageProtocol)
            m.AddCustomAttribute(Attributes.xrdRequiredHeaders context.MessageProtocol.HeaderNamespace operation.InputParameters.RequiredHeaders)
            attributes |> Seq.iter m.AddCustomAttribute
            if String.IsNullOrEmpty(doc) |> not then m.AddXmlDoc(doc)
            m

        additionalMembers.Add(meth)

        additionalMembers |> Seq.toList

/// Builds all types, namespaces and services for give producer definition.
/// Called by type provider to retrieve assembly details for generated types.
let makeProducerType (asm: ProvidedAssembly, ns, typeName) (uri, languageCode, operationFilter) =
    // Load schema details from specified file or network location.
    let schema = ProducerDescription.Load(resolveUri uri, languageCode, operationFilter)

    // Initialize type and schema element lookup context.
    let context = TypeBuilderContext.FromSchema(schema, languageCode)

    // Create base type which holds types generated from all provided schema-s.
    let serviceTypesTy =
        ProvidedTypeDefinition("DefinedTypes", Some typeof<obj>, isErased = false)

    // Create stubs for each type before building them, because of circular dependencies.
    schema.TypeSchemas
    |> Map.toList
    |> List.iter (fun (_,typeSchema) ->
        typeSchema.Types
        |> Seq.map (fun kvp -> SchemaType(kvp.Key))
        |> Seq.iter (context.GetOrCreateType >> ignore))

    (*
    // Build all global types for each type schema definition.
    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Types)
    |> Seq.choose (fun x ->
        match context.GetRuntimeType(SchemaType(x.Key)) with
        | CollectionType(prtyp, _, Some(st)) -> Some(prtyp, st)
        | CollectionType(_, _, None) -> None
        | rtyp -> Some(rtyp, x.Value))
    |> Seq.iter (fun (rtyp, def) -> TypeBuilder.build context rtyp def)
    *)

    // Main class that wraps all provided functionality and types.
    let targetClass =
        let t = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(serviceTypesTy)
        t

    // Create methods for all operation bindings.
    schema.Services
    |> List.iter (fun service ->
        let serviceTy = 
            ProvidedTypeDefinition(service.Name, Some typeof<obj>, isErased = false)

        service.Ports
        |> List.iter (fun port ->
            let portTy =
                let t = ProvidedTypeDefinition(port.Name, Some typeof<AbstractEndpointDeclaration>, isErased = false)
                port.Documentation |> Option.iter t.AddXmlDoc
                t

            let createCtors (initField: (ProvidedField * string) option) = [
                let baseCtor = typeof<AbstractEndpointDeclaration>.GetConstructor([| typeof<Uri> |])

                if Uri.IsWellFormedUriString(port.Uri, UriKind.Absolute) then
                    let c = ProvidedConstructor([], invokeCode = (fun args -> match initField with Some(f, v) -> Expr.FieldSet(Expr.Coerce(args.[0], portTy), f, Expr.Value(v)) | None -> <@@ () @@>))
                    c.BaseConstructorCall <- fun args -> (baseCtor, [ args.[0]; Expr.NewObject(typeof<Uri>.GetConstructor([| typeof<string> |]), [ Expr.Value(port.Uri) ]) ])
                    yield c

                let c2 = ProvidedConstructor([ProvidedParameter("uri", typeof<string>)], invokeCode = (fun args -> match initField with Some(f, v) -> Expr.FieldSet(Expr.Coerce(args.[0], portTy), f, Expr.Value(v)) | None -> <@@ () @@>))
                c2.BaseConstructorCall <- fun args -> (baseCtor, [ args.[0]; Expr.NewObject(typeof<Uri>.GetConstructor([| typeof<string> |]), [ args.[1] ]) ])
                yield c2

                let c3 = ProvidedConstructor([ProvidedParameter("uri", typeof<Uri>)], invokeCode = (fun args -> match initField with Some(f, v) -> Expr.FieldSet(Expr.Coerce(args.[0], portTy), f, Expr.Value(v)) | None -> <@@ () @@>))
                c3.BaseConstructorCall <- fun args -> (baseCtor, args)
                yield c3
            ]

            serviceTy.AddMember(portTy)

            // Create property and backing field for producer name.
            // By default service port xrd/xtee:address extension producer value is used, but user can override that value.
            match port.MessageProtocol.ProducerName with
            | Some(producerName) ->
                let producerField = ProvidedField("producerName", typeof<string>)
                let producerProperty =
                    ProvidedProperty(
                        "ProducerName",
                        typeof<string>,
                        getterCode = (fun args -> Expr.FieldGet(Expr.Coerce(args.[0], portTy), producerField)),
                        setterCode = (fun args -> Expr.FieldSet(Expr.Coerce(args.[0], portTy), producerField, args.[1]))
                    )

                portTy.AddMember(producerField)
                portTy.AddMember(producerProperty)
                portTy.AddMembers(createCtors (Some(producerField, producerName)))
            | None ->
                portTy.AddMembers(createCtors None)

            port.Methods
            |> List.iter (fun op -> portTy.AddMembers(ServiceBuilder.build context service.Namespace op))
        )

        targetClass.AddMember(serviceTy)
        )

    // Create types for all type namespaces.
    context.CachedNamespaces
    |> Seq.iter (fun kvp -> serviceTypesTy.AddMember(kvp.Value))

    asm.AddTypes([targetClass])

    targetClass
