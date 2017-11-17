﻿module internal XRoad.ProducerDefinition

open System
open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml
open XRoad.Serialization.Attributes
open XRoad.CodeDom
open XRoad.CodeDomGenerator
open XRoad.Wsdl
open XRoad.TypeSchema

/// Functions and types to handle building methods for services and operation bindings.
module ServiceBuilder =
    open XRoad.Serialization.Attributes

    /// Creates return type for the operation.
    /// To support returning multiple output parameters, they are wrapped into tuples accordingly:
    /// Single parameter responses return that single parameter.
    /// Multiple parameter responses are wrapped into tuples, since C# provides tuples upto 8 arguments,
    /// some composition is required when more output parameters are present.
    let private makeReturnType isMultipart (types: (string * RuntimeType) list) =
        let rec getReturnTypeTuple (tuple: (string * RuntimeType) list, types) =
            match types with
            | [] -> let typ = CodeTypeReference("System.Tuple", tuple |> List.map (fun (_,typ) -> typ.AsCodeTypeReference()) |> Array.ofList)
                    (typ, Expr.instOf typ (tuple |> List.map (fun (varName,_) -> !+ varName)))
            | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
            | x::xs -> let inner = getReturnTypeTuple([x], xs)
                       let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_,typ) -> typ.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
                       (typ, Expr.instOf typ ((tuple |> List.map (fun (varName,_) -> !+ varName)) @ [snd inner]))
        let types =
            if isMultipart
            then ("reader.Context.Attachments", PrimitiveType(typeof<IDictionary<string,Stream>>))::types
            else types
        match types with
        | [] -> (CodeTypeReference(typeof<Void>), Expr.empty)
        | [(varName, typ)] -> (typ.AsCodeTypeReference(), !+ varName)
        | many -> getReturnTypeTuple([], many)

    let instQN (nm: string) (ns: string) = Expr.inst<XmlQualifiedName> [!^ nm; !^ ns]

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
    let build (context: TypeBuilderContext) tns (operation: ServicePortMethod): CodeMemberMethod =
        let protocol = context.MessageProtocol.EnumValue
        let m =
            Meth.create operation.Name
            |> Meth.describe (Attributes.xrdOperation operation.Name operation.Version protocol context.MessageProtocol)
            |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
            |> Code.comment operation.Documentation
            |> Meth.describe (Attributes.xrdRequiredHeaders context.MessageProtocol.HeaderNamespace operation.InputParameters.RequiredHeaders)

        m |>
            match context.MessageProtocol with
            | Version20(_) -> Meth.addParam<XRoadRpcHeader> "header"
            | Version30(_) | Version31Ee(_) | Version31Eu(_) -> Meth.addParam<XRoadDocHeader> "header"
            | Version40(_) -> Meth.addParam<XRoadHeader> "header"
        |> ignore

        let argumentExpressions = ResizeArray<_>()

        let addDocLiteralWrappedParameters (spec: ElementSpec) =
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
                                let subTy = Cls.create (sprintf "%s_%sType" operation.Name name) |> Cls.addAttr TypeAttributes.Public |> Cls.describe (Attributes.xrdDefType LayoutKind.Sequence)
                                let ns = context.GetOrCreateNamespace(tns)
                                ns.Members.Add(subTy) |> ignore
                                let runtimeType = ProvidedType(subTy, providedTypeFullName ns.Name subTy.Name)
                                TypeBuilder.build context runtimeType definition
                                runtimeType
                            | Name(typeName) -> context.GetRuntimeType(SchemaType(typeName))
                        m |> Meth.addParamRef (runtimeType.AsCodeTypeReference(optional=(dspec.MinOccurs = 0u))) name |> ignore
                        argumentExpressions.Add((!+ name))
                    | _ -> failwithf "%A" value)
            | _ -> failwithf "Input wrapper element must be defined as complex type that is a sequence of elements (erroneous XML Schema entity `%s`)." (spec.Name |> Option.defaultValue "<unknown>")

        match operation.InputParameters with
//        | DocEncoded(encodingNamespace, wrapper) ->
//            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) (Some(encodingNamespace.NamespaceName)))
//        | DocLiteralBody(content) ->
//            addParameter content.Parameters.Head None None
        | DocLiteralWrapped(name, content) ->
            m |> Meth.describe (Attributes.xrdRequest name.LocalName name.NamespaceName false content.HasMultipartContent) |> ignore
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

        // buildOperationOutput context operation protocol result |> ignore
        match operation.OutputParameters with
        | DocLiteralWrapped(name, content) ->
            let runtimeType = TypeBuilder.buildResponseElementType context name
            m |> Meth.returnsOf (runtimeType.AsCodeTypeReference())
              |> Meth.addStmt
                (Stmt.ret
                    (Expr.cast
                        (runtimeType.AsCodeTypeReference())
                            ((Expr.typeRefOf<XRoad.XRoadUtil> @-> "MakeServiceCall")
                                @% [(Expr.this @-> "GetType") @% []
                                    !^ operation.Name
                                    Expr.this @=> "ProducerUri"
                                    Expr.this @=> "ProducerName"
                                    !+ "header"
                                    Arr.create (argumentExpressions |> Seq.toList)])))
              |> Meth.describe (Attributes.xrdResponse name.LocalName name.NamespaceName false content.HasMultipartContent)
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
        | _ -> m

/// Builds all types, namespaces and services for give producer definition.
/// Called by type provider to retrieve assembly details for generated types.
let makeProducerType (typeNamePath: string [], producerUri, languageCode) =
    // Load schema details from specified file or network location.
    let schema = ProducerDescription.Load(resolveUri producerUri, languageCode)

    // Initialize type and schema element lookup context.
    let context = TypeBuilderContext.FromSchema(schema, languageCode)

    // Create base type which holds types generated from all provided schema-s.
    let serviceTypesTy = Cls.create "DefinedTypes" |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

    // Create stubs for each type before building them, because of circular dependencies.
    schema.TypeSchemas
    |> Map.toList
    |> List.iter (fun (_,typeSchema) ->
        typeSchema.Types
        |> Seq.map (fun kvp -> SchemaType(kvp.Key))
        |> Seq.iter (context.GetOrCreateType >> ignore))

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

    // Main class that wraps all provided functionality and types.
    let targetClass =
        Cls.create typeNamePath.[typeNamePath.Length - 1]
        |> Cls.setAttr TypeAttributes.Public
        |> Cls.asStatic
        |> Cls.addMember serviceTypesTy

    // Create methods for all operation bindings.
    schema.Services
    |> List.iter (fun service ->
        let serviceTy = Cls.create service.Name |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic
        service.Ports
        |> List.iter (fun port ->
            // Create property and backing field for producer adapter server uri.
            // By default service port soap:address extension location value is used, but user can override that value.
            let addressField = Fld.create<string> "producerUri"
            let addressFieldRef = Expr.this @=> addressField.Name
            let addressProperty = CodeMemberProperty(Name="ProducerUri", Type=CodeTypeReference(typeof<string>))
            addressProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
            addressProperty.GetStatements.Add(Stmt.ret addressFieldRef) |> ignore
            addressProperty.SetStatements.Add(Stmt.assign addressFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

            // Create property and backing field for producer name.
            // By default service port xrd/xtee:address extension producer value is used, but user can override that value.
            let producerField = CodeMemberField(typeof<string>, "producerName")
            let producerFieldRef = Expr.this @=> producerField.Name
            let producerProperty = CodeMemberProperty(Name="ProducerName", Type=CodeTypeReference(typeof<string>))
            producerProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
            producerProperty.GetStatements.Add(Stmt.ret producerFieldRef) |> ignore
            producerProperty.SetStatements.Add(Stmt.assign producerFieldRef (CodePropertySetValueReferenceExpression())) |> ignore

            let ctor =
                Ctor.create()
                |> Ctor.setAttr MemberAttributes.Public
                |> Ctor.addStmt (Stmt.assign (Expr.this @=> "producerUri") (!^ port.Uri))

            // Add default producer name value if message protocol defines it in service description.
            port.MessageProtocol.ProducerName
            |> Option.iter (fun producerName -> ctor |> Ctor.addStmt (Stmt.assign (Expr.this @=> "producerName") (!^ producerName)) |> ignore)

            let portTy =
                Cls.create port.Name
                |> Cls.setAttr TypeAttributes.Public
                |> Cls.addMember ctor
                |> Cls.addMember addressField
                |> Cls.addMember addressProperty
                |> Cls.addMember producerField
                |> Cls.addMember producerProperty
                |> Code.comment port.Documentation
            serviceTy |> Cls.addMember portTy |> ignore

            port.Methods
            |> List.iter (fun op -> portTy |> Cls.addMember (ServiceBuilder.build context service.Namespace op) |> ignore))
        targetClass |> Cls.addMember serviceTy |> ignore
        )

    // Create types for all type namespaces.
    context.CachedNamespaces |> Seq.iter (fun kvp -> kvp.Value |> serviceTypesTy.Members.Add |> ignore)

    // Initialize default namespace to hold main type.
    let codeNamespace = CodeNamespace(String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1)))
    codeNamespace.Types.Add(targetClass) |> ignore

    // Compile the assembly and return to type provider.
    let assembly = Compiler.buildAssembly(codeNamespace)
    assembly.GetType(sprintf "%s.%s" codeNamespace.Name targetClass.Name)