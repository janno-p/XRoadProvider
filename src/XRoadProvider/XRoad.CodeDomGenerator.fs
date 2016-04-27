module internal XRoad.CodeDomGenerator

open System
open System.CodeDom
open System.Reflection
open System.Xml
open System.Xml.Linq
open XRoad.CodeDom
open XRoad.Serialization.Attributes
open XRoad.TypeSchema
open XRoad.Wsdl

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
                | Some(hasWrapper), CollectionType(_, itemName, _) ->
                    let isItemNillable = definition.IsItemNillable |> Option.fold (fun _ x -> x) false
                    prop |> Prop.describe (Attributes.xrdElement(None, None, definition.IsNillable, not hasWrapper))
                         |> Prop.describe (Attributes.xrdCollection(Some(itemName), isItemNillable))
                         |> ignore
                | Some(_), _ -> failwith "Array should match to CollectionType."
                | None, _ -> prop |> Prop.describe (Attributes.xrdElement(elementName, None, definition.IsNillable, false)) |> ignore
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

    let private nameGenerator name =
        let num = ref 0
        (fun () ->
            num := !num + 1
            sprintf "%s%d" name !num)

    let private buildEnumerationConstants (runtimeType: RuntimeType) (content: RestrictionContent list) =
        content
        |> List.choose (fun x ->
            match x with
            | Enumeration(value) ->
                Fld.createRef (runtimeType.AsCodeTypeReference(true)) (value.toPropertyName())
                |> Fld.setAttr (MemberAttributes.Public ||| MemberAttributes.Static)
                |> Fld.init (Expr.instOf (runtimeType.AsCodeTypeReference()) [!^ value])
                |> Some
            | _ -> None)

    /// Collects property definitions from every content element of complexType.
    let rec private collectComplexTypeContentProperties choiceNameGen seqNameGen context (spec: ComplexTypeContentSpec) =
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
                    | Choice(cspec) ->
                        collectChoiceProperties choiceNameGen context cspec
                    | Element(spec) ->
                        [ buildElementProperty context spec ]
                    | Sequence(sspec) ->
                        collectSequenceProperties seqNameGen context sspec
                    | Any ->
                        [ buildAnyProperty() ]
                    | Group ->
                        failwith "Not implemented: group in complexType sequence."
                spec.Content |> List.map (collectSequenceProperties) |> List.collect (id)
            | Some(ComplexTypeParticle.Choice(cspec)) ->
                collectChoiceProperties choiceNameGen context cspec
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
        let name, typeDefinition = context.GetAttributeDefinition(spec)
        // Resolve schema type for attribute:
        let schemaType =
            match typeDefinition with
            | Definition(simpleTypeSpec) -> Definition(SimpleDefinition(simpleTypeSpec))
            | Name(name) -> Name(name)
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
                let typ = Cls.create(name + suffix) |> Cls.addAttr TypeAttributes.Public |> Cls.describe (Attributes.xrdDefType LayoutKind.Sequence)
                let runtimeType = ProvidedType(typ, typ.Name)
                build context runtimeType def
                { propertyDef with
                    Type = CollectionType(runtimeType, itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    AddedTypes = [typ]
                    IsWrappedArray = Some(true) }
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

    /// Create property definitions for sequence element specification.
    and private collectSequenceProperties _ _ _ : PropertyDefinition list =
        []

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
                | Any ->
                    failwith "Not implemented: any in choice."
                | Choice(_) ->
                    failwith "Not implemented: choice in choice."
                | Element(spec) ->
                    let prop = buildElementProperty context spec
                    choiceType |> Cls.describe (Attributes.xrdChoiceOption (i + 1) prop.Name false) |> ignore
                    addNewMethod (i + 1) prop.Name prop.Type
                    addTryMethod (i + 1) prop.Name prop.Type
                    None
                | Group ->
                    failwith "Not implemented: group in choice."
                | Sequence(spec) ->
                    let props = buildSequenceMembers context spec
                    let optionName = optionNameGenerator()
                    choiceType |> Cls.describe (Attributes.xrdChoiceOption (i + 1) optionName true) |> ignore
                    let optionType = createOptionType optionName props
                    let optionRuntimeType = ProvidedType(optionType, optionType.Name)
                    addNewMethod (i + 1) optionName optionRuntimeType
                    addTryMethod (i + 1) optionName optionRuntimeType
                    Some(optionType))

        [{ PropertyDefinition.Create(choiceName, false, None) with Type = choiceRuntimeType; AddedTypes = choiceType::addedTypes }]

    /// Extract property definitions for all the elements defined in sequence element.
    and private buildSequenceMembers context (spec: ParticleSpec) =
        spec.Content
        |> List.map (function
            | Any -> failwith "Not implemented: any in sequence."
            | Choice(_) -> failwith "Not implemented: choice in sequence."
            | Element(espec) -> buildElementProperty context espec
            | Group -> failwith "Not implemented: group in sequence."
            | Sequence(_) -> failwith "Not implemented: sequence in sequence.")

    /// Populate generated type declaration with properties specified in type schema definition.
    and build (context: TypeBuilderContext) runtimeType schemaType =
        // Extract type declaration from runtime type definition.
        let providedTy =
            match runtimeType with
            | ProvidedType(decl,_) -> decl
            | _ -> failwith "Only generated types are accepted as arguments!"
        // Generates unique type name for every choice element.
        let choiceNameGen = nameGenerator "Choice"
        let seqNameGen = nameGenerator "Seq"
        // Parse schema definition and add all properties that are defined.
        match schemaType with
        | SimpleDefinition(SimpleTypeSpec.Restriction(spec, annotation)) ->
            providedTy |> Code.comment (annotationToText context annotation) |> ignore
            let values = spec.Content |> buildEnumerationConstants runtimeType
            values |> List.iter (providedTy.Members.Add >> ignore)
            match context.GetRuntimeType(SchemaType(spec.Base)) with
            | ContentType
            | PrimitiveType(_) as rtyp ->
                providedTy
                |> addContentProperty("BaseValue", rtyp)
                |> iif (values |> List.isEmpty) (fun x -> x |> Ctor.setAttr (MemberAttributes.Public))
                |> ignore
                Ctor.create() |> providedTy.Members.Add |> ignore
            | _ -> failwith "Simple types should not restrict complex types."
        | SimpleDefinition(ListDef) ->
            failwith "Not implemented: list in simpleType."
        | SimpleDefinition(Union(_)) ->
            failwith "Not implemented: union in simpleType."
        | ComplexDefinition(spec) ->
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
                    | ProvidedType(_) as baseTy -> providedTy |> Cls.setParent (baseTy.AsCodeTypeReference()) |> ignore
                    | _ -> failwithf "Only complex types can be inherited! (%A)" spec.Base
                    Some(spec.Content)
                | ComplexContent(ComplexContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s complexContent"
                | ComplexTypeContent.Particle(spec) ->
                    Some(spec)
                | ComplexTypeContent.Empty ->
                    None
            specContent
            |> Option.fold (fun _ content -> providedTy |> addTypeProperties (collectComplexTypeContentProperties choiceNameGen seqNameGen context content)) ()
        | EmptyDefinition -> ()

    let removeFaultDescription (definition: SchemaTypeDefinition) =
        let filterFault (particles: ParticleContent list) =
            particles
        match definition with
        | ComplexDefinition({ Content = Particle({ Content = Some(ComplexTypeParticle.Sequence(sequence)) } as particle) } as spec) ->
            let newParticle =
                match sequence.Content with
                | [ ParticleContent.Choice(choice) ] ->
                    match choice.Content |> filterFault with
                    | [] | [_] as content -> ComplexTypeParticle.Sequence({ choice with Content = content })
                    | content -> ComplexTypeParticle.Choice({ choice with Content = content })
                | content -> ComplexTypeParticle.Sequence({ sequence with Content = filterFault content })
            ComplexDefinition({ spec with Content = Particle({ particle with Content = Some(newParticle) }) })
        | EmptyDefinition | ComplexDefinition(_) | SimpleDefinition(_) -> definition

    let buildResponseElementType (context: TypeBuilderContext) (elementName: XName) =
        let elementSpec = elementName |> context.GetElementSpec
        match elementSpec.Definition with
        | Explicit(typeDefinition) ->
            match typeDefinition with
            | Definition(definition) ->
                let runtimeType = context.GetOrCreateType(SchemaElement(elementName))
                definition |> removeFaultDescription |> build context runtimeType
                runtimeType
            | Name(typeName) ->
                context.GetRuntimeType(SchemaType(typeName))
        | Reference(_) -> failwith "Root level element references are not allowed."
