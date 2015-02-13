module internal XRoad.ProducerDefinitionCodeDom

open Microsoft.CSharp
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq
open System.Xml.Serialization
open XRoadTypeProvider.Wsdl
open XRoadTypeProvider.Wsdl.XsdSchema

type private RuntimeType =
    | PrimitiveType of Type
    | ProvidedType of CodeTypeReference
    member this.AsCodeTypeReference() = match this with
                                        | PrimitiveType(typ) -> CodeTypeReference(typ)
                                        | ProvidedType(typ) -> typ

let private compileAssembly code =
    let fileName = Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> sprintf "%A.dll")
    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(OutputAssembly=fileName, GenerateExecutable=false)
    let compilerResults = codeProvider.CompileAssemblyFromDom(parameters, [| code |])
    printfn "%A" compilerResults.Errors
    compilerResults.CompiledAssembly

let private makeStaticClass(className, attributes) =
    let targetClass = CodeTypeDeclaration(className)
    targetClass.IsClass <- true
    targetClass.TypeAttributes <- attributes
    targetClass.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" Environment.NewLine)) |> ignore
    targetClass.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
    targetClass

let private makePublicClass name =
    CodeTypeDeclaration(name, IsClass=true, TypeAttributes=TypeAttributes.Public)

let makeProducerType (typeNamePath: string []) producerUri =
    let schema = resolveUri producerUri |> readSchema
    let typeCache = Dictionary<XName,CodeTypeDeclaration>()
    let namespaceCache = Dictionary<XNamespace,CodeTypeDeclaration>()

    let serviceTypesTy = makeStaticClass("DefinedTypes", TypeAttributes.Public)

    let getOrCreateNamespace (name: XNamespace) =
        match namespaceCache.TryGetValue(name) with
        | false, _ ->
            let producerName = 
                match Regex.Match(name.NamespaceName, @"^http://producers\.\w+\.xtee\.riik\.ee/producer/(\w+)$") with
                | m when m.Success -> m.Groups.[1].Value
                | _ -> failwithf "TODO: Implement normal namespace handling for tns: %A" name
            let typ = CodeTypeDeclaration(producerName, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTypesTy.Members.Add(typ) |> ignore
            namespaceCache.Add(name, typ)
            typ
        | true, typ -> typ

    let getOrCreateType (name: XName) =
        match typeCache.TryGetValue(name) with
        | false, _ ->
            let typ = makePublicClass(name.LocalName)
            let namespaceTy = getOrCreateNamespace(name.Namespace)
            namespaceTy.Members.Add(typ) |> ignore
            typeCache.Add(name, typ)
            typ
        | true, typ -> typ

    let getRuntimeType typeName =
        match mapPrimitiveType typeName with
        | Some tp -> PrimitiveType(tp)
        | _ -> ProvidedType(CodeTypeReference(getOrCreateType(typeName).Name))

    let makeArrayType(typ, rank) =
        match typ with
        | PrimitiveType(typ) ->
            [1..rank] |> List.fold (fun (aggTyp: Type) _ -> aggTyp.MakeArrayType()) typ |> PrimitiveType
        | ProvidedType(typ) ->
            [1..rank] |> List.fold (fun (aggTyp: CodeTypeReference) _ -> CodeTypeReference(aggTyp, 1)) typ |> ProvidedType

    let (|SoapEncArray|_|) (typ: SchemaType) =
        let getArrayType arrayType =
            match arrayType with
            | Some(typeName, rank) -> makeArrayType(getRuntimeType(typeName), rank)
            | _ -> failwith "Array underlying type specification is missing."

        match typ with
        | SchemaType.ComplexType(spec) ->
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

    let rec buildType(providedTy: CodeTypeDeclaration, typeInfo) =
        let addProperty(name, ty: RuntimeType, isOptional) =
            let specifiedField =
                if isOptional then
                    let f = CodeMemberField(typeof<bool>, name + "__specified")
                    providedTy.Members.Add(f) |> ignore
                    Some(f)
                else None
            let backingField = CodeMemberField(ty.AsCodeTypeReference(), name + "__backing")
            providedTy.Members.Add(backingField) |> ignore
            let backingFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), backingField.Name)
            let property = CodeMemberProperty(Name=name, Type=ty.AsCodeTypeReference())
            property.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
            property.GetStatements.Add(CodeMethodReturnStatement(backingFieldRef)) |> ignore
            property.SetStatements.Add(CodeAssignStatement(backingFieldRef, CodePropertySetValueReferenceExpression())) |> ignore
            match specifiedField with
            | Some(field) ->
                let fieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), field.Name)
                property.SetStatements.Add(CodeAssignStatement(fieldRef, CodePrimitiveExpression(true))) |> ignore
            | _ -> ()
            providedTy.Members.Add(property) |> ignore

        let getParticleType (particleType, maxOccurs, isNillable, name) =
            match particleType with
            | RefOrType.Name(xname) ->
                let typ = getRuntimeType(xname)
                match typ with
                | x when maxOccurs > 1u -> makeArrayType(x, 1)
                | PrimitiveType(x) when isNillable && x.IsValueType -> PrimitiveType(typedefof<Nullable<_>>.MakeGenericType(x))
                | x -> x
            | RefOrType.Type(SoapEncArray(typ)) ->
                typ
            | RefOrType.Type(typeInfo) ->
                let subTy = makePublicClass(name + "Type")
                buildType(subTy, typeInfo)
                providedTy.Members.Add(subTy) |> ignore
                let subTyRef = ProvidedType(CodeTypeReference(subTy.Name))
                if maxOccurs > 1u then makeArrayType(subTyRef, 1)
                else subTyRef
            | _ -> failwithf "not implemented: %A" name

        let parseElementSpec(spec: ElementSpec) =
            let elementTy = getParticleType(spec.Type, spec.MaxOccurs, spec.IsNillable, spec.Name)
            addProperty(spec.Name, elementTy, spec.MinOccurs = 0u)

        let parseComplexTypeContentSpec(spec: ComplexTypeContentSpec) =
            spec.Attributes |> List.iter (fun spec ->
                match spec.Name with
                | Some(name) ->
                    let attributeTy = getParticleType(spec.RefOrType, 1u, false, name)
                    addProperty(name, attributeTy, match spec.Use with Required -> true | _ -> false)
                | _ -> failwith "not implemented")
            match spec.Content with
            | Some(ComplexTypeParticle.All(spec)) ->
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then
                    failwith "not implemented"
                spec.Elements |> List.iter parseElementSpec
            | Some(ComplexTypeParticle.Sequence(spec)) ->
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then
                    failwith "not implemented"
                spec.Content |> List.iter (fun item ->
                    match item with
                    | SequenceContent.Element(spec) ->
                        parseElementSpec(spec)
                    | SequenceContent.Sequence(spec) ->
                        failwith "not implemented")
            | None -> ()

        match typeInfo with
        | SoapEncArray _ ->
            // TODO: Some global types like message responses are actually array-s
            ()
        | SimpleType(SimpleTypeSpec.Restriction(spec)) -> failwith "not implemented"
        | ComplexType(spec) ->
            if spec.IsAbstract then
                providedTy.TypeAttributes <- providedTy.TypeAttributes ||| TypeAttributes.Abstract
                providedTy.Members.Add(CodeConstructor(Attributes=MemberAttributes.Family)) |> ignore
            match spec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                match getRuntimeType(spec.Base) with
                | PrimitiveType(typ) as rtyp ->
                    addProperty("BaseValue", rtyp, false)
                    parseComplexTypeContentSpec(spec.Content)
                | ProvidedType(_) ->
                    failwith "not implemented"
            | SimpleContent(SimpleContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                let baseTy = getOrCreateType(spec.Base)
                providedTy.BaseTypes.Add(baseTy.Name) |> ignore
                let attribute = CodeAttributeDeclaration(CodeTypeReference(typeof<XmlIncludeAttribute>))
                attribute.Arguments.Add(CodeAttributeArgument(CodeTypeOfExpression(providedTy.Name))) |> ignore
                baseTy.CustomAttributes.Add(attribute) |> ignore
                parseComplexTypeContentSpec(spec.Content)
            | ComplexContent(ComplexContentSpec.Restriction(spec)) ->
                failwith "not implemented"
            | ComplexTypeContent.Particle(spec) ->
                parseComplexTypeContentSpec(spec)

    schema.TypeSchemas
    |> Seq.collect (fun typeSchema -> typeSchema.Types)
    |> Seq.map (fun x -> getOrCreateType(x.Key), x.Value)
    |> Seq.iter buildType

    let targetClass = makeStaticClass(typeNamePath.[typeNamePath.Length - 1], TypeAttributes.Public)
    targetClass.Members.Add(serviceTypesTy) |> ignore

    schema.Services |> List.iter (fun service ->
        let serviceTy = makeStaticClass(service.Name, TypeAttributes.Public)
        service.Ports |> List.iter (fun port ->
            let portTy = CodeTypeDeclaration(port.Name, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTy.Members.Add(portTy) |> ignore

            match port.Documentation.TryGetValue("et") with
            | true, doc -> portTy.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
            | _ -> ()

            let addressField = CodeMemberField(typeof<string>, "address", InitExpression=CodePrimitiveExpression(port.Address))
            let addressFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), addressField.Name)
            let addressProperty = CodeMemberProperty(Name="Address", Type=CodeTypeReference(typeof<string>))
            addressProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
            addressProperty.GetStatements.Add(CodeMethodReturnStatement(addressFieldRef)) |> ignore
            addressProperty.SetStatements.Add(CodeAssignStatement(addressFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

            portTy.Members.Add(addressField) |> ignore
            portTy.Members.Add(addressProperty) |> ignore

            let producerField = CodeMemberField(typeof<string>, "producer", InitExpression=CodePrimitiveExpression(port.Producer))
            let producerFieldRef = CodeFieldReferenceExpression(CodeThisReferenceExpression(), producerField.Name)
            let producerProperty = CodeMemberProperty(Name="Producer", Type=CodeTypeReference(typeof<string>))
            producerProperty.Attributes <- MemberAttributes.Public ||| MemberAttributes.Final
            producerProperty.GetStatements.Add(CodeMethodReturnStatement(producerFieldRef)) |> ignore
            producerProperty.SetStatements.Add(CodeAssignStatement(producerFieldRef, CodePropertySetValueReferenceExpression())) |> ignore

            portTy.Members.Add(producerField) |> ignore
            portTy.Members.Add(producerProperty) |> ignore

            // TODO: Implement service calls!!
            port.Operations |> List.map (fun op -> ()) |> ignore)
        targetClass.Members.Add(serviceTy) |> ignore)

    let codeNamespace = CodeNamespace(String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1)))
    codeNamespace.Types.Add(targetClass) |> ignore

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore
    codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore

    compileAssembly(codeCompileUnit).GetType(sprintf "%s.%s" codeNamespace.Name targetClass.Name)
