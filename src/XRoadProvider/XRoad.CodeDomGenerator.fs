module internal XRoad.CodeDomGenerator

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
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
          AddedTypes: ProvidedTypeDefinition list
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
            definition.Documentation |> Option.iter prop.AddXmlDoc
            let elementName = if prop.Name <> definition.Name then Some(definition.Name) else None
            if definition.IsIgnored then
                prop.AddCustomAttribute(Attributes.mkXmlIgnore())
            elif definition.IsAny then
                prop.AddCustomAttribute(Attributes.mkXmlAnyElement())
            elif definition.IsAttribute then
                prop.AddCustomAttribute(Attributes.mkXmlAttribute())
            else
                match definition.IsWrappedArray, definition.Type with
                | Some(hasWrapper), CollectionType(_, itemName, _) ->
                    let isItemNillable = definition.IsItemNillable |> Option.fold (fun _ x -> x) false
                    prop.AddCustomAttribute(Attributes.mkXrdElement(None, None, definition.IsNillable, not hasWrapper))
                    prop.AddCustomAttribute(Attributes.mkXrdCollection(Some(itemName), isItemNillable))
                | Some(_), _ ->
                    failwith "Array should match to CollectionType."
                | None, _ ->
                    prop.AddCustomAttribute(Attributes.mkXrdElement(elementName, None, definition.IsNillable, false))
            // Add extra types to owner type declaration.
            definition.AddedTypes |> List.iter (fun x -> ownerTy.AddMember(x))
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

    let private buildEnumerationConstants (runtimeType: RuntimeType) (itemType: RuntimeType) (content: RestrictionContent list) =
        let valueExpr (value: string) =
            match itemType with
            | PrimitiveType(t) when t = typeof<int32> -> Expr.Value(Convert.ToInt32(value))
            | _ -> Expr.Value(value)
        content
        |> List.choose (fun x ->
            match x with
            | Enumeration(value) ->
                let fld = ProvidedField(value.ToPropertyName(), runtimeType.AsCodeTypeReference(true))
                fld.SetFieldAttributes(FieldAttributes.Public ||| FieldAttributes.InitOnly ||| FieldAttributes.Static)
                // Fld.init (Expr.instOf (runtimeType.AsCodeTypeReference()) [valueExpr value])
                Some(fld, valueExpr(value))
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
                    | Choice(cspec) -> collectChoiceProperties choiceNameGen context cspec
                    | Element(spec) -> [ buildElementProperty context spec ]
                    | Sequence(sspec) -> collectSequenceProperties seqNameGen context sspec
                    | Any -> [ buildAnyProperty() ]
                    | Group -> failwith "Not implemented: group in complexType sequence."
                spec.Content |> List.map (collectSequenceProperties) |> List.collect (id)
            | Some(ComplexTypeParticle.Choice(cspec)) ->
                collectChoiceProperties choiceNameGen context cspec
            | Some(ComplexTypeParticle.Group) ->
                failwith "Not implemented: group in complexType."
            | None -> []
        List.concat [attributeProperties; elementProperties]

    /// Create single property definition for given element-s schema specification.
    and private buildElementProperty (context: TypeBuilderContext) (spec: ElementSpec) =
        let dspec, schemaType = context.DereferenceElementSpec(spec)
        let name = dspec.Name |> Option.get
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
            match context.DereferenceElementSpec(itemSpec) with
            | dspec, Name(n) ->
                let itemName = dspec.Name |> Option.get
                { propertyDef with
                    Type = CollectionType(context.GetRuntimeType(SchemaType(n)), itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    IsWrappedArray = Some(true) }
            | dspec, Definition(def) ->
                let itemName = dspec.Name |> Option.get
                let suffix = itemName.ToClassName()
                let typ = ProvidedTypeDefinition(name + suffix, Some(typeof<obj>), isErased = false)
                typ.AddCustomAttribute(Attributes.mkXrdDefType LayoutKind.Sequence)
                let runtimeType = ProvidedType(typ, typ.Name)
                build context runtimeType def
                { propertyDef with
                    Type = CollectionType(runtimeType, itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    AddedTypes = [typ]
                    IsWrappedArray = Some(true) }
        | Definition(def) ->
            let subTy = ProvidedTypeDefinition(name + "Type", Some(typeof<obj>), isErased = false)
            subTy.AddCustomAttribute(Attributes.mkXrdDefType LayoutKind.Sequence)
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
        let idField = ProvidedField("__id", typeof<int>)
        let valueField = ProvidedField("__value", typeof<obj>)

        let ctor =
            ProvidedConstructor(
                [ ProvidedParameter("id", typeof<int>)
                  ProvidedParameter("value", typeof<obj>) ],
                invokeCode = (fun args -> Expr.Sequential(Expr.FieldSet(idField, args.[0]), Expr.FieldSet(valueField, args.[1]))))
        // ctor.MethodAttributes(MemberAttributes.Private)

        let choiceName = choiceNameGenerator()
        let choiceType = ProvidedTypeDefinition(choiceName + "Type", Some(typeof<obj>), isErased = false)
        choiceType.AddCustomAttribute(Attributes.mkXrdDefType LayoutKind.Choice)
        choiceType.AddMembers([idField; valueField])
        choiceType.AddMember(ctor)

        let choiceRuntimeType = ProvidedType(choiceType, choiceType.Name)

        let createOptionType name (propList: PropertyDefinition list) =
            let optionType = ProvidedTypeDefinition(name + "Type", Some(typeof<obj>), isErased = false)
            optionType.AddCustomAttribute(Attributes.mkXrdDefType LayoutKind.Sequence)
            addTypeProperties propList optionType
            optionType

        let addTryMethod (id: int) (name: string) (runtimeType: RuntimeType) =
            let tryMethod =
                let rty = runtimeType.AsCodeTypeReference()
                ProvidedMethod(
                    sprintf "TryGet%s%s" (if Char.IsLower(name.[0]) then "_" else "") name,
                    [ ProvidedParameter("value", rty, isOut = true) ],
                    typeof<bool>,
                    invokeCode =
                        (fun args ->
                            Expr.Sequential(
                                Expr.VarSet(Var("value", rty), Expr.Value(if rty.IsClass then null else Activator.CreateInstance(rty))),
                                Expr.Sequential(
                                    Expr.IfThenElse(
                                        <@@ (%%(Expr.FieldGet(idField)): int32) = id @@>,
                                        Expr.VarSet(Var("value", rty), Expr.FieldGet(valueField)),
                                        Expr.Value(())),
                                    <@@ (%%(Expr.FieldGet(idField)): int32) = id @@>
                                )) 
                            ))
            choiceType.AddMember(tryMethod)

        let addNewMethod id (name: string) (runtimeType: RuntimeType) =
            let newMethod =
                let rty = runtimeType.AsCodeTypeReference()
                ProvidedMethod(
                    sprintf "New%s%s" (if Char.IsLower(name.[0]) then "_" else "") name,
                    [ ProvidedParameter("value", rty) ],
                    choiceRuntimeType.AsCodeTypeReference(),
                    isStatic = true,
                    invokeCode = (fun args -> Expr.NewObject(ctor, [Expr.Value(id); args.[0]])))
            choiceType.AddMember(newMethod)

        let optionNameGenerator = nameGenerator (sprintf "%sOption" choiceName)

        let addedTypes =
            spec.Content
            |> List.mapi (fun i x -> (i, x))
            |> List.choose (fun (i, choiceContent) ->
                match choiceContent with
                | Element(spec) ->
                    let prop = buildElementProperty context spec
                    choiceType.AddCustomAttribute(Attributes.mkXrdChoiceOption (i + 1) prop.Name false)
                    addNewMethod (i + 1) prop.Name prop.Type
                    addTryMethod (i + 1) prop.Name prop.Type
                    None
                | Sequence(spec) ->
                    let props = buildSequenceMembers context spec
                    let optionName = optionNameGenerator()
                    choiceType.AddCustomAttribute(Attributes.mkXrdChoiceOption (i + 1) optionName true)
                    let optionType = createOptionType optionName props
                    let optionRuntimeType = ProvidedType(optionType, optionType.Name)
                    addNewMethod (i + 1) optionName optionRuntimeType
                    addTryMethod (i + 1) optionName optionRuntimeType
                    Some(optionType)
                | Any -> failwith "Not implemented: any in choice."
                | Choice(_) -> failwith "Not implemented: choice in choice."
                | Group -> failwith "Not implemented: group in choice.")

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
            annotationToText context annotation |> Option.iter providedTy.AddXmlDoc
            match context.GetRuntimeType(SchemaType(spec.Base)) with
            | ContentType
            | PrimitiveType(_) as rtyp ->
                let values = buildEnumerationConstants runtimeType rtyp spec.Content
                values |> List.iter (fst >> providedTy.AddMember >> ignore)
                providedTy
                |> addContentProperty("BaseValue", rtyp)
                //|> iif (values |> List.isEmpty) (fun x -> x |> Ctor.setAttr (MemberAttributes.Public))
                |> ignore
                let ct = ProvidedConstructor([], invokeCode = (fun args -> <@@ () @@>))
                providedTy.AddMember(ct)
            | _ -> failwith "Simple types should not restrict complex types."
        | SimpleDefinition(ListDef) ->
            failwith "Not implemented: list in simpleType."
        | SimpleDefinition(Union(_)) ->
            failwith "Not implemented: union in simpleType."
        | ComplexDefinition(spec) ->
            // Abstract types will have only protected constructor.
            if spec.IsAbstract then
                providedTy.SetAttributes(TypeAttributes.Abstract)
                let ct = ProvidedConstructor([], invokeCode = (fun _ -> <@@ () @@>))
                // MemberAttributes.Family
                providedTy.AddMember(ct)
                annotationToText context spec.Annotation |> Option.iter providedTy.AddXmlDoc
            // Handle complex type content and add properties for attributes and elements.
            let specContent =
                match spec.Content with
                | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | PrimitiveType(_)
                    | ContentType as rtyp ->
                        let prop = providedTy |> addProperty ("BaseValue", rtyp, false)
                        prop.AddCustomAttribute(Attributes.mkXrdContent())
                        Some(spec.Content)
                    | _ ->
                        failwith "ComplexType-s simpleContent should not extend complex types."
                | SimpleContent(SimpleContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s simpleContent."
                | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | ProvidedType(_) as baseTy -> providedTy.SetBaseType(baseTy.AsCodeTypeReference())
                    | _ -> failwithf "Only complex types can be inherited! (%A)" spec.Base
                    Some(spec.Content)
                | ComplexContent(ComplexContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s complexContent"
                | ComplexTypeContent.Particle(spec) ->
                    Some(spec)
                | ComplexTypeContent.Empty ->
                    None
            specContent
            |> Option.fold (fun _ content -> addTypeProperties (collectComplexTypeContentProperties choiceNameGen seqNameGen context content) providedTy) ()
        | EmptyDefinition -> ()

    let removeFaultDescription (definition: SchemaTypeDefinition) =
        let isFault content =
            let areFaultElements (el1: ElementSpec) (el2: ElementSpec) =
                el1.Name = Some("faultCode") && el2.Name = Some("faultString")
            match content with
            | Sequence({ Content = [Element(el1); Element(el2)] }) -> areFaultElements el1 el2 || areFaultElements el2 el1
            | _ -> false
        let filterFault (particles: ParticleContent list) =
            particles |> List.filter (isFault >> not)
        match definition with
        | ComplexDefinition({ Content = Particle({ Content = Some(ComplexTypeParticle.Sequence(sequence)) } as particle) } as spec) ->
            let newParticle =
                match sequence.Content with
                | [ ParticleContent.Choice(choice) ] ->
                    match choice.Content |> filterFault with
                    | [Sequence(content)] -> ComplexTypeParticle.Sequence(content)
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
