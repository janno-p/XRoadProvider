module internal XRoad.ProducerDefinition

open System
open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml
open XRoad.Serialization.Attributes
open XRoad.CodeDom
open XRoad.Wsdl
open XRoad.TypeSchema

/// Functions and types to handle type building process.
module TypeBuilder =
    /// Describes single property for type declaration.
    type private PropertyDefinition =
        { /// Name of the property.
          Name: string
          /// Runtime type to use on property.
          Type: RuntimeType
          /// Does property accept nil values?
          IsNillable: bool
          /// Can array items be nil values?
          IsItemNillable: bool option
          /// Extra types to add as nested type declarations to owner type.
          AddedTypes: CodeTypeDeclaration list
          /// Can property value be unspecified in resulting SOAP message.
          IsOptional: bool
          /// Does array type property specify wrapper element around items?
          IsWrappedArray: bool option
          // Attribute type:
          IsAttribute: bool
          IsAny: bool
          IsIgnored: bool
          // Documentation tooltips
          Documentation: string option }
        /// Initializes default property with name and optional value.
        static member Create(name, isOptional, doc) =
            { Type = RuntimeType.PrimitiveType(typeof<System.Void>)
              IsNillable = false
              IsItemNillable = None
              AddedTypes = []
              IsOptional = isOptional
              IsWrappedArray = None
              Name = name
              IsAttribute = false
              IsAny = false
              IsIgnored = false
              Documentation = doc }

    /// Build property declarations from property definitions and add them to owner type.
    let private addTypeProperties definitions ownerTy =
        let addTypePropertiesFromDefinition definition =
            // Most of the conditions handle XmlSerializer specific attributes.
            let prop = ownerTy |> addProperty(definition.Name, definition.Type, definition.IsOptional)
                               |> Code.comment (definition.Documentation)
            let elementName = if prop.Name <> definition.Name then Some(definition.Name) else None
            if definition.IsIgnored then
                prop |> Prop.describe Attributes.XmlIgnore |> ignore
            elif definition.IsAny then
                prop |> Prop.describe Attributes.XmlAnyElement |> ignore
            elif definition.IsAttribute then
                prop |> Prop.describe Attributes.XmlAttribute |> ignore
            else
                match definition.IsWrappedArray, definition.Type with
                | Some(true), CollectionType(_, itemName, _) ->
                    prop |> Prop.describe (Attributes.xrdElement(None, None, definition.IsNillable))
                         |> Prop.describe (Attributes.xrdCollection(Some(itemName), definition.IsItemNillable.Value))
                         |> ignore
                | Some(true), _ ->
                    failwith "Wrapped array should match to CollectionType."
                | (None | Some(false)), _ ->
                    prop |> Prop.describe (Attributes.xrdElement(elementName, None, definition.IsNillable)) |> ignore
            // Add extra types to owner type declaration.
            definition.AddedTypes |> List.iter (fun x -> ownerTy |> Cls.addMember x |> ignore)
        definitions |> List.iter (addTypePropertiesFromDefinition)

    /// Create definition of property that accepts any element not defined in schema.
    let private buildAnyProperty () =
        let prop = PropertyDefinition.Create("AnyElements", false, None)
        { prop with Type = PrimitiveType(typeof<XmlElement[]>); IsAny = true }

    let private annotationToText (context: TypeBuilderContext) (annotation: Annotation option) =
        annotation
        |> Option.bind (fun annotation ->
            annotation.AppInfo
            |> List.collect (fun e -> e.Elements(titleElementName context.MessageProtocol) |> List.ofSeq)
            |> List.fold (fun doc el ->
                let lang = el |> attrOrDefault (xnsname "lang" XmlNamespace.Xml) "et"
                (lang, el.Value)::doc) []
            |> List.tryFind (fst >> ((=) context.LanguageCode))
            |> Option.map snd)

    let nameGenerator name =
        let num = ref 0
        (fun () ->
            num := !num + 1
            sprintf "%s%d" name !num)

    /// Populate generated type declaration with properties specified in type schema definition.
    let rec build (context: TypeBuilderContext) runtimeType schemaType =
        // Extract type declaration from runtime type definition.
        let providedTy, _ (*providedTypeName*) =
            match runtimeType with
            | ProvidedType(decl, name) -> decl, name
            | _ -> failwith "Only generated types are accepted as arguments!"
        // Generates unique type name for every choice element.
        let choiceNameGenerator = nameGenerator "Choice"
        // Parse schema definition and add all properties that are defined.
        match schemaType with
        | SimpleType(SimpleTypeSpec.Restriction(spec, annotation)) ->
            providedTy |> Code.comment (annotationToText context annotation) |> ignore
            match context.GetRuntimeType(SchemaType(spec.Base)) with
            | ContentType
            | PrimitiveType(_) as rtyp ->
                providedTy |> addProperty("BaseValue", rtyp, false) |> Prop.describe Attributes.xrdContent |> ignore
            | _ ->
                failwith "Simple types should not restrict complex types."
        | SimpleType(ListDef) ->
            failwith "Not implemented: list in simpleType."
        | SimpleType(Union(_)) ->
            failwith "Not implemented: union in simpleType."
        | ComplexType(spec) ->
            // Abstract types will have only protected constructor.
            if spec.IsAbstract then
                providedTy |> Cls.addAttr TypeAttributes.Abstract
                           |> Cls.addMember (Ctor.create() |> Ctor.setAttr MemberAttributes.Family)
                           |> Code.comment (annotationToText context spec.Annotation)
                           |> ignore
            // Handle complex type content and add properties for attributes and elements.
            let specContent =
                match spec.Content with
                | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | PrimitiveType(_)
                    | ContentType as rtyp ->
                        providedTy |> addProperty("BaseValue", rtyp, false) |> Prop.describe Attributes.xrdContent |> ignore
                        Some(spec.Content)
                    | _ ->
                        failwith "ComplexType-s simpleContent should not extend complex types."
                | SimpleContent(SimpleContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s simpleContent."
                | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | ProvidedType(_ (*baseDecl*),_) as baseTy ->
                        providedTy |> Cls.setParent (baseTy.AsCodeTypeReference()) |> ignore
                    | _ ->
                        failwithf "Only complex types can be inherited! (%A)" spec.Base
                    Some(spec.Content)
                | ComplexContent(ComplexContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s complexContent"
                | ComplexTypeContent.Particle(spec) ->
                    Some(spec)
                | ComplexTypeContent.Empty ->
                    None
            specContent
            |> Option.fold (fun _ content -> providedTy |> addTypeProperties (collectComplexTypeContentProperties choiceNameGenerator context content)) ()
        | EmptyType -> ()

    /// Collects property definitions from every content element of complexType.
    and private collectComplexTypeContentProperties choiceNameGenerator context spec =
        // Attribute definitions
        let attributeProperties = spec.Attributes |> List.map (buildAttributeProperty context)
        // Element definitions
        let elementProperties =
            match spec.Content with
            | Some(ComplexTypeParticle.All(spec)) ->
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
                spec.Elements |> List.map (buildElementProperty context)
            | Some(ComplexTypeParticle.Sequence(spec)) ->
                if spec.MinOccurs > 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
                let collectSequenceProperties content =
                    match content with
                    | SequenceContent.Choice(cspec) ->
                        collectChoiceProperties choiceNameGenerator context cspec
                    | SequenceContent.Element(spec) ->
                        [ buildElementProperty context spec ]
                    | SequenceContent.Sequence(_) ->
                        failwith "Not implemented: sequence in complexType sequence."
                    | SequenceContent.Any ->
                        [ buildAnyProperty() ]
                    | SequenceContent.Group ->
                        failwith "Not implemented: group in complexType sequence."
                spec.Content |> List.map (collectSequenceProperties) |> List.collect (id)
            | Some(ComplexTypeParticle.Choice(cspec)) ->
                collectChoiceProperties choiceNameGenerator context cspec
            | Some(ComplexTypeParticle.Group) ->
                failwith "Not implemented: group in complexType."
            | None -> []
        List.concat [attributeProperties; elementProperties]

    /// Create single property definition for given element-s schema specification.
    and private buildElementProperty (context: TypeBuilderContext) (spec: ElementSpec) =
        let name, schemaType = context.GetElementDefinition(spec)
        buildPropertyDef schemaType spec.MaxOccurs name spec.IsNillable (spec.MinOccurs = 0u) context (annotationToText context spec.Annotation)

    /// Create single property definition for given attribute-s schema specification.
    and private buildAttributeProperty (context: TypeBuilderContext) (spec: AttributeSpec) =
        let name, schemaObject = context.GetAttributeDefinition(spec)
        // Resolve schema type for attribute:
        let schemaType =
            match schemaObject with
            | Definition(simpleTypeSpec) -> Definition(SimpleType(simpleTypeSpec))
            | Name(name) -> Name(name)
            | Reference(ref) -> Reference(ref)
        let isOptional = match spec.Use with Required -> true | _ -> false
        let prop = buildPropertyDef schemaType 1u name false isOptional context (annotationToText context spec.Annotation)
        { prop with IsAttribute = true }

    /// Build default property definition from provided schema information.
    and private buildPropertyDef schemaType maxOccurs name isNillable isOptional context doc =
        let propertyDef = PropertyDefinition.Create(name, isOptional, doc)
        match schemaType with
        | Definition(ArrayContent itemSpec) ->
            match context.GetElementDefinition(itemSpec) with
            | itemName, Name(n) ->
                { propertyDef with
                    Type = CollectionType(context.GetRuntimeType(SchemaType(n)), itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    IsWrappedArray = Some(true) }
            | itemName, Definition(def) ->
                let suffix = itemName.toClassName()
                let typ = Cls.create(name + suffix) |> Cls.addAttr TypeAttributes.Public
                let runtimeType = ProvidedType(typ, typ.Name)
                build context runtimeType def
                { propertyDef with
                    Type = CollectionType(runtimeType, itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    AddedTypes = [typ]
                    IsWrappedArray = Some(true) }
            | _, Reference(_) -> failwith "never"
        | Definition(def) ->
            let subTy = Cls.create (name + "Type") |> Cls.addAttr TypeAttributes.Public |> Cls.describe (Attributes.xrdDefType LayoutKind.Sequence)
            let runtimeType = ProvidedType(subTy, subTy.Name)
            build context runtimeType def
            if maxOccurs > 1u then
                { propertyDef with
                    Type = CollectionType(runtimeType, name, None)
                    IsNillable = isNillable
                    AddedTypes = [subTy]
                    IsWrappedArray = Some(false) }
            else
                { propertyDef with
                    Type = runtimeType
                    IsNillable = isNillable
                    AddedTypes = [subTy] }
        | Name(n) ->
            match context.GetRuntimeType(SchemaType(n)) with
            | x when maxOccurs > 1u ->
                { propertyDef with
                    Type = CollectionType(x, name, None)
                    IsNillable = isNillable
                    IsWrappedArray = Some(false) }
            | PrimitiveType(x) when x.IsValueType ->
                { propertyDef with
                    Type = PrimitiveType(if isNillable then typedefof<Nullable<_>>.MakeGenericType(x) else x)
                    IsNillable = isNillable }
            | x ->
                { propertyDef with
                    Type = x
                    IsNillable = isNillable }
        | Reference(_) ->
            failwith "Not implemented: schema reference to type."

    /// Create property definitions for choice element specification.
    and private collectChoiceProperties choiceNameGenerator context spec : PropertyDefinition list =
        let idField = Fld.create<int> "__id"
        let valueField = Fld.create<obj> "__value"

        let ctor =
            Ctor.create()
            |> Ctor.setAttr MemberAttributes.Private
            |> Ctor.addParam<int> "id"
            |> Ctor.addParam<obj> "value"
            |> Ctor.addStmt (Stmt.assign (Expr.this @=> "__id") (!+ "id"))
            |> Ctor.addStmt (Stmt.assign (Expr.this @=> "__value") (!+ "value"))

        let choiceName = choiceNameGenerator()
        let choiceType =
            Cls.create (choiceName + "Type")
            |> Cls.setAttr (TypeAttributes.Public ||| TypeAttributes.Sealed)
            |> Cls.describe (Attributes.xrdDefType LayoutKind.Choice)
            |> Cls.addMembers [idField; valueField; ctor]

        let choiceRuntimeType = ProvidedType(choiceType, choiceType.Name)

        let createOptionType name (propList: PropertyDefinition list) =
            let optionType =
                Cls.create (name + "Type")
                |> Cls.describe (Attributes.xrdDefType LayoutKind.Sequence)
            optionType |> addTypeProperties propList
            optionType

        let addTryMethod (id: int) (name: string) (runtimeType: RuntimeType) =
            let tryMethod =
                Meth.create (sprintf "TryGet%s%s" (if Char.IsLower(name.[0]) then "_" else "") name)
                |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
                |> Meth.returns<bool>
                |> Meth.addOutParamRef (runtimeType.AsCodeTypeReference()) "value"
                |> Meth.addStmt (Stmt.assign (!+ "value") (Expr.defaultValue (runtimeType.AsCodeTypeReference())))
                |> Meth.addStmt (Stmt.condIf (Op.equals (Expr.this @=> "__id") (!^ id))
                                             [Stmt.assign (!+ "value") (Expr.cast (runtimeType.AsCodeTypeReference()) (Expr.this @=> "__value"))])
                |> Meth.addStmt (Stmt.ret (Op.equals (Expr.this @=> "__id") (!^ id)))
            choiceType |> Cls.addMember(tryMethod) |> ignore

        let addNewMethod id (name: string) (runtimeType: RuntimeType) =
            let newMethod =
                Meth.create (sprintf "New%s%s" (if Char.IsLower(name.[0]) then "_" else "") name)
                |> Meth.setAttr (MemberAttributes.Static ||| MemberAttributes.Public)
                |> Meth.returnsOf (choiceRuntimeType.AsCodeTypeReference())
                |> Meth.addParamRef (runtimeType.AsCodeTypeReference()) "value"
                |> Meth.addStmt (Stmt.ret (Expr.instOf (choiceRuntimeType.AsCodeTypeReference()) [!^ id; !+ "value"]))
            choiceType |> Cls.addMember(newMethod) |> ignore

        let optionNameGenerator = nameGenerator (sprintf "%sOption" choiceName)

        let addedTypes =
            spec.Content
            |> List.mapi (fun i x -> (i, x))
            |> List.choose (fun (i, choiceContent) ->
                match choiceContent with
                | ChoiceContent.Any ->
                    failwith "Not implemented: any in choice."
                | ChoiceContent.Choice(_) ->
                    failwith "Not implemented: choice in choice."
                | ChoiceContent.Element(spec) ->
                    let prop = buildElementProperty context spec
                    choiceType |> Cls.describe (Attributes.xrdChoiceOption (i + 1) prop.Name false) |> ignore
                    addNewMethod (i + 1) prop.Name prop.Type
                    addTryMethod (i + 1) prop.Name prop.Type
                    None
                | ChoiceContent.Group ->
                    failwith "Not implemented: group in choice."
                | ChoiceContent.Sequence(spec) ->
                    let props = buildSequenceMembers context spec
                    let optionName = optionNameGenerator()
                    choiceType |> Cls.describe (Attributes.xrdChoiceOption (i + 1) optionName true) |> ignore
                    let optionType = createOptionType optionName props
                    let optionRuntimeType = ProvidedType(optionType, optionType.Name)
                    addNewMethod (i + 1) optionName optionRuntimeType
                    addTryMethod (i + 1) optionName optionRuntimeType
                    Some(optionType))

        let choiceProperty =
            let prop = PropertyDefinition.Create(choiceName, false, None)
            { prop with Type = choiceRuntimeType; AddedTypes = choiceType::addedTypes }

        [choiceProperty]

        (*
            let isArray = options |> List.map (List.length) |> List.max > 1
            let enumNameType =
                let rt = ProvidedType(choiceEnum, choiceEnum.Name)
                if isArray then CollectionType(rt, "", None) else rt
            let choiceTypeProp =
                let prop = PropertyDefinition.Create(choiceName + "Name", false, None)
                { prop with Type = enumNameType; IsIgnored = true; AddedTypes = [choiceEnum] }
            // Create property for holding option values.
            let choiceItemType =
                let rt =
                    options
                    |> List.collect (id)
                    |> List.fold (fun (s: RuntimeType option) x ->
                        match s with
                        | None -> Some(x.Type)
                        | Some(y) when x.Type = y -> s
                        | _ -> Some(PrimitiveType(typeof<obj>))) None
                    |> Option.get
                if isArray then CollectionType(rt, "", None) else rt
            let choiceElements = options |> List.collect (id)
            choiceElements
            |> List.iter (fun opt ->
                let fld =
                    Fld.createEnum (choiceName + "Type") opt.Name
                    |> Fld.describe (Attributes.XmlEnum opt.Name)
                choiceEnum
                |> Cls.addMember fld
                |> ignore)
            let choiceItemProp =
                let prop = PropertyDefinition.Create(choiceName + (if isArray then "Items" else"Item"), false, None)
                { prop with Type = choiceItemType; ChoiceIdentifier = Some(choiceTypeProp.Name); ChoiceElements = choiceElements }
            [ choiceTypeProp; choiceItemProp ]
        *)

    /// Extract property definitions for all the elements defined in sequence element.
    and private buildSequenceMembers context (spec: SequenceSpec) =
        spec.Content
        |> List.map (
            function
            | SequenceContent.Any ->
                failwith "Not implemented: any in sequence."
            | SequenceContent.Choice(_) ->
                failwith "Not implemented: choice in sequence."
            | SequenceContent.Element(espec) ->
                buildElementProperty context espec
            | SequenceContent.Group ->
                failwith "Not implemented: group in sequence."
            | SequenceContent.Sequence(_) ->
                failwith "Not implemented: sequence in sequence.")

/// Functions and types to handle building methods for services and operation bindings.
module ServiceBuilder =
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

    let addHeaderInitialization messageProtocol serviceName (reqhdrs: string list) (m: CodeMemberMethod) =
        let props, headerType =
            match messageProtocol with
            | Version20(_) ->
                m |> Meth.addParam<XRoadRpcHeader> "header" |> ignore
                Some("Andmekogu", "Nimi"), typeRef<XRoadRpcHeader>
            | Version30(_) | Version31Ee(_) | Version31Eu(_) ->
                m |> Meth.addParam<XRoadDocHeader> "header" |> ignore
                Some("Producer", "Service"), typeRef<XRoadDocHeader>
            | Version40(_) ->
                m |> Meth.addParam<XRoadHeader> "header" |> ignore;
                None, typeRef<XRoadHeader>
        m |> Meth.addStmt (Stmt.condIf (Op.isNull (!+ "header")) [Stmt.assign (!+ "header") (Expr.instOf headerType [])]) |> ignore
        match props with
        | Some(producerPropName, servicePropName) ->
            m |> Meth.addStmt (Stmt.condIf ((Expr.typeRefOf<string> @-> "IsNullOrWhiteSpace") @% [!+ "header" @=> producerPropName]) [Stmt.assign (!+ "header" @=> producerPropName) (!^ messageProtocol.ProducerName.Value)])
              |> Meth.addStmt (Stmt.condIf ((Expr.typeRefOf<string> @-> "IsNullOrWhiteSpace") @% [!+ "header" @=> servicePropName]) [Stmt.assign (!+ "header" @=> servicePropName) ((Expr.typeRefOf<string> @-> "Format") @% [!^ (sprintf "{0}.%s" serviceName); !+ "header" @=> producerPropName])])
              |> ignore
        | None ->
            m |> Meth.addStmt (Stmt.condIf ((Expr.typeRefOf<string> @-> "IsNullOrWhiteSpace") @% [!+ "header" @=> "ProtocolVersion"]) [Stmt.assign (!+ "header" @=> "ProtocolVersion") (!^ "4.0")])
              |> ignore
        m |> Meth.addStmt (Stmt.assign (!+ "@__m" @=> "RequiredHeaders") (reqhdrs |> List.map (fun x -> !^ x) |> Arr.create<string>))
          |> Meth.addStmt (Stmt.assign ((!+ "@__m") @=> "Header") (!+ "header"))
          |> Meth.addStmt (Stmt.assign (!+ "@__m" @=> "HeaderNamespace") (!^ messageProtocol.HeaderNamespace))

    let instQN (nm: string) (ns: string) = Expr.inst<XmlQualifiedName> [!^ nm; !^ ns]

    let buildOperationInput (context: TypeBuilderContext) (operation: ServicePortMethod) (paramClass: CodeTypeDeclaration) m =
        m |> Meth.addStmt (CodeVariableDeclarationStatement(paramClass.Name, "@__input", CodeObjectCreateExpression(paramClass.Name)))
          |> Meth.addStmt (Stmt.assign (!+ "@__m" @=> "Body") (!+ "@__input"))
          |> ignore
        let namespaceSet = SortedSet<_>()
        let addParameter (parameter: Parameter) nm ns =
            let runtimeType = context.GetRuntimeType(match parameter.Type with Some(typeName) -> SchemaType(typeName) | None -> SchemaElement(parameter.Name))
            m |> Meth.addParamRef (runtimeType.AsCodeTypeReference()) parameter.Name.LocalName |> ignore
            let prop = paramClass |> addProperty (parameter.Name.LocalName, runtimeType, false)
            let attr = Attributes.xrdElement (nm, ns, false)
            prop |> Prop.describe (match nm, ns with None, None -> attr |> Attr.addNamedArg "MergeContent" (!^ true) | _ -> attr) |> ignore
            match runtimeType with
            | CollectionType(x, itemName, y) -> prop |> Prop.describe (Attributes.xrdCollection(Some(itemName), true)) |> ignore
            | _ -> ()
            m |> Meth.addStmt(Stmt.assign (!+ "@__input" @=> parameter.Name.LocalName) (!+ parameter.Name.LocalName)) |> ignore
            ns |> Option.iter (fun ns -> if (not (String.IsNullOrWhiteSpace(ns))) && namespaceSet.Add(ns) then m |> Meth.addExpr (((!+ "@__m" @=> "Namespaces") @-> "Add") @% [!^ ns]) |> ignore)
        match operation.InputParameters with
        | DocEncodedCall(encodingNamespace, wrapper) ->
            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) (Some(encodingNamespace.NamespaceName)))
        | DocLiteralCall({ Parameters = [{ Type = Some(_) } as p] }) ->
            addParameter p None None
        | DocLiteralCall(wrapper) ->
            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) (Some(p.Name.NamespaceName)))
        | RpcEncodedCall(accessor, wrapper) ->
            m |> Meth.addStmt (Stmt.assign (!+ "@__reqOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
        | RpcLiteralCall(accessor, { Parameters = [{ Type = Some(_) } as p] }) ->
            addParameter p (Some(accessor.LocalName)) (Some(accessor.NamespaceName))
        | RpcLiteralCall(accessor, wrapper) ->
            m |> Meth.addStmt (Stmt.assign (!+ "@__reqOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
            wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
        m

    let buildOperationOutput (context: TypeBuilderContext) (operation: ServicePortMethod) (resultClass: CodeTypeDeclaration) m =
        let addParameter (parameter: Parameter) nm ns =
            let runtimeType = context.GetRuntimeType(match parameter.Type with Some(typeName) -> SchemaType(typeName) | None -> SchemaElement(parameter.Name))
            let prop = resultClass |> addProperty (parameter.Name.LocalName, runtimeType, false)
            let attr = Attributes.xrdElement (nm, ns, false)
            prop |> Prop.describe (match nm, ns with None, None -> attr |> Attr.addNamedArg "MergeContent" (!^ true) | _ -> attr) |> ignore
        let parameters =
            match operation.OutputParameters with
            | DocEncodedCall(encodingNamespace, wrapper) ->
                wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) (Some(encodingNamespace.NamespaceName)))
                []
            | DocLiteralCall({ Parameters = [{ Type = Some(_) } as p] }) ->
                addParameter  p None None
                []
            | DocLiteralCall(wrapper) ->
                wrapper.Parameters
                |> List.map (fun p ->
                    addParameter p (Some(p.Name.LocalName)) (Some(p.Name.NamespaceName))
                    (p.Name, context.GetRuntimeType(SchemaElement(p.Name))))
            | RpcEncodedCall(accessor, wrapper) ->
                m |> Meth.addStmt (Stmt.assign (!+ "@__respOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
                wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
                []
            | RpcLiteralCall(accessor, { Parameters = [{ Type = Some(_) } as p] }) ->
                addParameter p (Some(accessor.LocalName)) (Some(accessor.NamespaceName))
                []
            | RpcLiteralCall(accessor, wrapper) ->
                m |> Meth.addStmt (Stmt.assign (!+ "@__respOpt" @=> "Accessor") (instQN accessor.LocalName accessor.NamespaceName)) |> ignore
                wrapper.Parameters |> List.iter (fun p -> addParameter p (Some(p.Name.LocalName)) None)
                []
        m |> Meth.addStmt (Stmt.declVarWith<XRoad.XRoadMessage> "@__r" ((Expr.typeRefOf<XRoad.XRoadUtil> @-> "MakeServiceCall") @% [!+ "@__m"; !+ "@__reqOpt"; !+ "@__respOpt"])) |> ignore
        match parameters with
        | [] ->
            m |> Meth.addStmt (Stmt.ret Expr.empty)
        | [(name, typ)] ->
            m |> Meth.returnsOf (typ.AsCodeTypeReference())
              |> Meth.addStmt (Stmt.ret ((Expr.cast (CodeTypeReference(resultClass.Name)) ((!+ "@__r") @=> "Body")) @=> name.LocalName))
        | _ ->
            m |> Meth.returns<obj>
              |> Meth.addStmt (Stmt.ret Expr.nil)

    /// Build content for each individual service call method.
    let build (context: TypeBuilderContext) _ (operation: ServicePortMethod): CodeTypeMember list =
        let protocol = context.MessageProtocol.EnumValue
        let paramClass = Cls.create(sprintf "%sInput" operation.Name) |> Cls.setAttr (TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed) |> Cls.describe Attributes.xrdRoot
        let resultClass = Cls.create(sprintf "%sOutput" operation.Name) |> Cls.setAttr (TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed) |> Cls.describe Attributes.xrdRoot
        let m =
            Meth.create operation.Name
            |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
            |> Code.comment operation.Documentation
            |> Meth.addStmt (Stmt.declVarWith<XRoad.XRoadMessage> "@__m" (Expr.inst<XRoad.XRoadMessage> []))
            |> Meth.addStmt (Stmt.declVarWith<XRoad.XRoadRequestOptions> "@__reqOpt" (Expr.inst<XRoad.XRoadRequestOptions> [Expr.this @=> "ProducerUri"; !^ operation.InputParameters.IsEncoded; !^ operation.InputParameters.IsMultipart; Expr.typeRefOf<XRoad.XRoadProtocol> @=> protocol.ToString()]))
            |> Meth.addStmt (Stmt.assign (!+ "@__reqOpt" @=> "ServiceCode") (!^ operation.Name))
            |> iif operation.Version.IsSome (fun x -> x |> Meth.addStmt (Stmt.assign (!+ "@__reqOpt" @=> "ServiceVersion") (!^ operation.Version.Value)))
            |> Meth.addStmt (Stmt.declVarWith<XRoad.XRoadResponseOptions> "@__respOpt" (Expr.inst<XRoad.XRoadResponseOptions> [!^ operation.OutputParameters.IsEncoded; !^ operation.OutputParameters.IsMultipart; Expr.typeRefOf<XRoad.XRoadProtocol> @=> protocol.ToString(); Expr.typeOf (CodeTypeReference(resultClass.Name)) ]))
            |> addHeaderInitialization context.MessageProtocol (match operation.Version with Some v -> sprintf "%s.%s" operation.Name v | _ -> operation.Name) operation.InputParameters.RequiredHeaders
            |> buildOperationInput context operation paramClass
            |> buildOperationOutput context operation resultClass
        [m; paramClass; resultClass]

/// Builds all types, namespaces and services for give producer definition.
/// Called by type provider to retrieve assembly details for generated types.
let makeProducerType (typeNamePath: string [], producerUri, undescribedFaults, languageCode) =
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
        |> Seq.append (
            typeSchema.Elements
            |> Seq.choose (fun kvp ->
                match kvp.Value.Type with
                | Definition(_) -> Some(SchemaElement(kvp.Key))
                | _ -> None))
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

    // Build all global elements for each type schema definition.
    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Elements)
    |> Seq.choose (fun x ->
        match x.Value.Type with
        | Definition(_) -> Some(context.GetRuntimeType(SchemaElement(x.Key)), x.Value)
        | _ -> None)
    |> Seq.iter (fun (typ, spec) ->
        match spec.Type with
        | Definition(def) -> TypeBuilder.build context typ def
        | Reference(_) -> failwith "Root level element references are not allowed."
        | Name(_) -> ())

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
            |> List.iter (fun op -> portTy |> Cls.addMembers (ServiceBuilder.build context undescribedFaults op) |> ignore))
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
