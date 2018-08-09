namespace XRoad

open CodeDom
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System
open System.Reflection
open System.Xml.Linq
open TypeSchema
open Wsdl
open XRoad.Choices
open XRoad.Serialization.Attributes

/// Functions and types to handle type building process.
module internal TypeBuilder =
    /// Describes single property for type declaration.
    type PropertyDefinition =
        { /// Name of the property.
          Name: string
          /// Runtime type to use on property.
          Type: RuntimeType
          /// Does property accept nil values?
          IsNillable: bool
          /// Can array items be nil values?
          IsItemNillable: bool option
          /// Can property value be unspecified in resulting SOAP message.
          IsOptional: bool
          /// Does array type property specify wrapper element around items?
          IsWrappedArray: bool option
          // Attribute type:
          IsAttribute: bool
          IsAny: bool
          IsIgnored: bool
          // Documentation tooltips
          Documentation: string option
          UseXop: bool }
        /// Initializes default property with name and optional value.
        static member Create(name, isOptional, doc, useXop) =
            { Type = PrimitiveType(typeof<Void>)
              IsNillable = false
              IsItemNillable = None
              IsOptional = isOptional
              IsWrappedArray = None
              Name = name
              IsAttribute = false
              IsAny = false
              IsIgnored = false
              Documentation = doc
              UseXop = useXop }

    let private getAttributesForProperty idx elementName (prop: PropertyDefinition) =
        match prop.IsWrappedArray, prop.Type with
        | Some(hasWrapper), CollectionType(_,itemName,_) ->
            let isItemNillable = prop.IsItemNillable |> MyOption.defaultValue false
            [ Attributes.xrdElement idx elementName None prop.IsNillable (not hasWrapper) prop.UseXop
              Attributes.xrdCollection idx (Some(itemName)) None isItemNillable false ]
        | Some(_), _ ->
            failwith "Array should match to CollectionType."
        | None, _ ->
            [ Attributes.xrdElement idx elementName None prop.IsNillable false prop.UseXop ]

    /// Build property declarations from property definitions and add them to owner type.
    let private addTypeProperties (definitions, subTypes) ownerTy =
        let addTypePropertiesFromDefinition definition =
            // Most of the conditions handle XmlSerializer specific attributes.
            let prop = ownerTy |> addProperty(definition.Name, definition.Type, definition.IsOptional)
            definition.Documentation |> Option.iter prop.AddXmlDoc
            let elementName = if prop.Name <> definition.Name then Some(definition.Name) else None
            if definition.IsIgnored then
                prop.AddCustomAttribute(Attributes.XmlIgnore)
            elif definition.IsAny then
                prop.AddCustomAttribute(Attributes.XmlAnyElement)
            elif definition.IsAttribute then
                prop.AddCustomAttribute(Attributes.XmlAttribute)
            else
                definition |> getAttributesForProperty None elementName |> List.iter prop.AddCustomAttribute 
        definitions |> List.iter (addTypePropertiesFromDefinition)
        // Add extra types to owner type declaration.
        ownerTy.AddMembers(subTypes)

    /// Create definition of property that accepts any element not defined in schema.
    let private buildAnyProperty () =
        let prop = PropertyDefinition.Create("AnyElements", false, None, false)
        { prop with Type = PrimitiveType(typeof<XElement[]>); IsAny = true }

    let private annotationToText (context: TypeBuilderContext) (annotation: Annotation option) =
        annotation
        |> Option.bind (fun annotation ->
            annotation.AppInfo
            |> List.collect (fun e -> e.Elements(titleElementName context.MessageProtocol) |> List.ofSeq)
            |> List.fold (fun doc el ->
                let lang = el |> attrOrDefault (X.name "lang" XmlNamespace.Xml) "et"
                (lang, el.Value)::doc) []
            |> List.tryFind (fst >> ((=) context.LanguageCode))
            |> Option.map snd)

    let nameGenerator name =
        let num = ref 0
        (fun () ->
            num := !num + 1
            sprintf "%s%d" name !num)

    let private buildEnumerationType (spec: SimpleTypeRestrictionSpec, itemType) (providedTy: ProvidedTypeDefinition) =
        let initCtor = providedTy |> addContentProperty("BaseValue", itemType, false, spec.Content |> List.isEmpty |> not)

        let valueExpr (value: string) =
            let v =
                match itemType with 
                | PrimitiveType(t) when t = typeof<int32> -> Expr.Value(Convert.ToInt32(value))
                | _ -> Expr.Value(value)
            Expr.NewObject(initCtor, [v])

        let staticCtorExpr =
            spec.Content
            |> List.map (fun x ->
                match x with 
                | Enumeration(value) ->
                    let nm = value.GetValidPropertyName()
                    let f = ProvidedField(false, nm + "__backing", FieldAttributes.Private ||| FieldAttributes.Static, providedTy, null, K [| |])
                    let p = ProvidedProperty(nm, providedTy, getterCode = (fun _ -> Expr.FieldGet(f)), isStatic = true)
                    providedTy.AddMember(f)
                    providedTy.AddMember(p)
                    Expr.FieldSet(f, valueExpr value)
                | _ -> failwithf "Enumeration types restriction content is not implemented: %s" (x.GetType().Name)
            )
            |> List.reduce (fun a b -> Expr.Sequential(a, b))

        providedTy.AddMember(ProvidedConstructor([], (fun _ -> staticCtorExpr), IsTypeInitializer = true))
        providedTy.AddMember(ProvidedConstructor(false, MethodAttributes.Private ||| MethodAttributes.RTSpecialName, [| |], (fun _ -> <@@ () @@>), None, false, K [| |]))

    let getChoiceInterface len =
        match len with 
        | 1 -> Some(typedefof<IChoiceOf1<_>>)
        | 2 -> Some(typedefof<IChoiceOf2<_,_>>)
        | 3 -> Some(typedefof<IChoiceOf3<_,_,_>>)
        | 4 -> Some(typedefof<IChoiceOf4<_,_,_,_>>)
        | 5 -> Some(typedefof<IChoiceOf5<_,_,_,_,_>>)
        | 6 -> Some(typedefof<IChoiceOf6<_,_,_,_,_,_>>)
        | 7 -> Some(typedefof<IChoiceOf7<_,_,_,_,_,_,_>>)
        | 8 -> Some(typedefof<IChoiceOf8<_,_,_,_,_,_,_,_>>)
        | _ -> None

    /// Collects property definitions from every content element of complexType.
    let rec private collectComplexTypeContentProperties choiceNameGen seqNameGen context (spec: ComplexTypeContentSpec) =
        // Attribute definitions
        let attributeProperties, attrTypes =
            spec.Attributes 
            |> List.fold (fun (xs, ys) n -> let x, y = n |> buildAttributeProperty context in x::xs, y |> List.append ys) ([], [])
        // Element definitions
        let elementProperties, elemTypes =
            match spec.Content with
            | Some(All(spec)) ->
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
                spec.Elements
                |> List.map (buildElementProperty context)
                |> List.unzip
                |> (fun (a, b) -> a, b |> List.collect id)
            | Some(ComplexTypeParticle.Sequence(spec)) ->
                if spec.MinOccurs > 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
                let collectSequenceProperties content =
                    match content with
                    | Choice(cspec) -> let x, ts = collectChoiceProperties choiceNameGen context cspec in [x], ts
                    | Element(spec) -> let x, ts = buildElementProperty context spec in [x], ts
                    | Sequence(sspec) -> (collectSequenceProperties seqNameGen context sspec), []
                    | Any -> [ buildAnyProperty() ], []
                    | Group -> failwith "Not implemented: group in complexType sequence."
                spec.Content |> List.fold (fun (xs, ys) n -> let x, y = n |> collectSequenceProperties in x |> List.append xs, y |> List.append ys) ([], [])
            | Some(ComplexTypeParticle.Choice(cspec)) ->
                let prop, types = collectChoiceProperties choiceNameGen context cspec
                [prop], types
            | Some(ComplexTypeParticle.Group) ->
                failwith "Not implemented: group in complexType."
            | None -> [], []
        (List.concat [attributeProperties; elementProperties], List.concat [attrTypes; elemTypes])

    /// Create single property definition for given element-s schema specification.
    and private buildElementProperty (context: TypeBuilderContext) (spec: ElementSpec) : PropertyDefinition * ProvidedTypeDefinition list =
        let dspec, schemaType = context.DereferenceElementSpec(spec)
        let name = dspec.Name |> Option.get
        buildPropertyDef schemaType spec.MaxOccurs name spec.IsNillable (spec.MinOccurs = 0u) context (annotationToText context spec.Annotation) spec.ExpectedContentTypes.IsSome

    /// Create single property definition for given attribute-s schema specification.
    and private buildAttributeProperty (context: TypeBuilderContext) (spec: AttributeSpec) : PropertyDefinition * ProvidedTypeDefinition list =
        let name, typeDefinition = context.GetAttributeDefinition(spec)
        // Resolve schema type for attribute:
        let schemaType =
            match typeDefinition with
            | Definition(simpleTypeSpec) -> Definition(SimpleDefinition(simpleTypeSpec))
            | Name(name) -> Name(name)
        let isOptional = match spec.Use with Required -> true | _ -> false
        let prop, types = buildPropertyDef schemaType 1u name false isOptional context (annotationToText context spec.Annotation) false
        { prop with IsAttribute = true }, types

    /// Build default property definition from provided schema information.
    and private buildPropertyDef schemaType maxOccurs name isNillable isOptional context doc useXop : PropertyDefinition * ProvidedTypeDefinition list =
        match schemaType with
        | Definition(ArrayContent itemSpec) ->
            match context.DereferenceElementSpec(itemSpec) with
            | dspec, Name(n) ->
                let itemName = dspec.Name |> Option.get
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = CollectionType(context.GetRuntimeType(SchemaType(n)), itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    IsWrappedArray = Some(true) }, [])
            | dspec, Definition(def) ->
                let itemName = dspec.Name |> Option.get
                let suffix = itemName.ToClassName()
                let typ = ProvidedTypeDefinition(name + suffix, Some typeof<obj>, isErased = false)
                typ.AddCustomAttribute(Attributes.xrdAnonymousType LayoutKind.Sequence)
                let runtimeType = ProvidedType(typ, typ.Name)
                build context runtimeType def
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = CollectionType(runtimeType, itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    IsWrappedArray = Some(true) }, [typ])
        | Definition(def) ->
            let subTy = ProvidedTypeDefinition(name + "Type", Some typeof<obj>, isErased = false)
            subTy.AddCustomAttribute(Attributes.xrdAnonymousType LayoutKind.Sequence)
            let runtimeType = ProvidedType(subTy, subTy.Name)
            build context runtimeType def
            if maxOccurs > 1u then
                ({ PropertyDefinition.Create(name, false, doc, useXop) with
                    Type = CollectionType(runtimeType, name, None)
                    IsNillable = isNillable
                    IsWrappedArray = Some(false) }, [subTy])
            else
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = runtimeType
                    IsNillable = isNillable }, [subTy])
        | Name(n) ->
            match context.GetRuntimeType(SchemaType(n)) with
            | x when maxOccurs > 1u ->
                ({ PropertyDefinition.Create(name, false, doc, useXop) with
                    Type = CollectionType(x, name, None)
                    IsNillable = isNillable
                    IsWrappedArray = Some(false) }, [])
            | PrimitiveType(x) when x.IsValueType ->
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = PrimitiveType(if isNillable then typedefof<Nullable<_>>.MakeGenericType(x) else x)
                    IsNillable = isNillable }, [])
            | x ->
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = x
                    IsNillable = isNillable }, [])

    /// Create property definitions for sequence element specification.
    and private collectSequenceProperties _ _ _ : PropertyDefinition list =
        []

    and collectChoiceProperties choiceNameGenerator context spec : PropertyDefinition * ProvidedTypeDefinition list =
        PropertyDefinition.Create("tere", false, None, false), []

    (*
    /// Create property definitions for choice element specification.
    and collectChoiceProperties choiceNameGenerator context spec : PropertyDefinition * CodeTypeDeclaration list =
        let idField = Fld.create<int> "__id"
        let valueField = Fld.create<obj> "__value"

        let ctor =
            Ctor.create()
            |> Ctor.setAttr MemberAttributes.Private
            |> Ctor.addParam<int> "id"
            |> Ctor.addParam<obj> "value"
            |> Ctor.addStmt (Stmt.assign (Expr.this @=> "__id") (!+ "id"))
            |> Ctor.addStmt (Stmt.assign (Expr.this @=> "__value") (!+ "value"))

        let choiceInterface = getChoiceInterface spec.Content.Length

        let choiceName = choiceNameGenerator()
        let choiceType =
            Cls.create (choiceName + "Type")
            |> iif choiceInterface.IsSome (Cls.implements choiceInterface.Value)
            |> Cls.setAttr (TypeAttributes.Public ||| TypeAttributes.Sealed)
            |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Choice)
            |> Cls.addMembers [idField; valueField; ctor]

        let choiceRuntimeType = ProvidedType(choiceType, choiceType.Name)

        let createOptionType name (propList: PropertyDefinition list) =
            let optionType =
                Cls.create (name + "Type")
                |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Sequence)
            optionType |> addTypeProperties (propList, [])
            optionType

        let addTryMethod (id: int) (methName: string) (runtimeType: RuntimeType) =
            let tryMethod =
                Meth.create methName
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

        let addChoiceMethod i mname (t: CodeTypeReference) =
            choiceInterface
            |> Option.iter
                (fun x ->
                    x.TypeArguments.Add(t) |> ignore
                    let m =
                        Meth.create (sprintf "TryGetOption%d" i)
                        |> Meth.returns<bool>
                        |> Meth.addOutParamRef t "value"
                        |> Meth.addStmt (Stmt.ret ((Expr.this @-> mname) @% [!+ "out value"]))
                    m.PrivateImplementationType <- x
                    choiceType |> Cls.addMember m |> ignore)

        let addedTypes =
            spec.Content
            |> List.mapi (fun i choiceContent ->
                let methName (name: string) =
                    sprintf "TryGet%s%s" (if Char.IsLower(name.[0]) then "_" else "") name
                match choiceContent with
                | Element(spec) ->
                    let prop, types = buildElementProperty context spec
                    prop |> getAttributesForProperty (Some(i + 1)) (Some(prop.Name)) |> List.iter (fun attr -> choiceType |> Cls.describe attr |> ignore)
                    addNewMethod (i + 1) prop.Name prop.Type
                    let name = methName prop.Name
                    addTryMethod (i + 1) name prop.Type
                    addChoiceMethod (i + 1) name (prop.Type.AsCodeTypeReference())
                    types
                | Sequence(spec) ->
                    let props, types = buildSequenceMembers context spec
                    let optionName = optionNameGenerator()
                    choiceType |> Cls.describe (Attributes.xrdElement (Some(i + 1)) (Some(optionName)) None false true false) |> ignore
                    let optionType = createOptionType optionName props
                    let optionRuntimeType = ProvidedType(optionType, optionType.Name)
                    addNewMethod (i + 1) optionName optionRuntimeType
                    let name = methName optionName
                    addTryMethod (i + 1) name optionRuntimeType
                    addChoiceMethod (i + 1) name (optionRuntimeType.AsCodeTypeReference())
                    optionType::types
                | Any -> failwith "Not implemented: any in choice."
                | Choice(_) -> failwith "Not implemented: choice in choice."
                | Group -> failwith "Not implemented: group in choice.")
            |> List.collect id

        { PropertyDefinition.Create(choiceName, false, None, false) with Type = choiceRuntimeType }, choiceType::addedTypes

    /// Extract property definitions for all the elements defined in sequence element.
    and private buildSequenceMembers context (spec: ParticleSpec) : PropertyDefinition list * CodeTypeDeclaration list =
        spec.Content
        |> List.map (function
            | Any -> failwith "Not implemented: any in sequence."
            | Choice(_) -> failwith "Not implemented: choice in sequence."
            | Element(espec) -> buildElementProperty context espec
            | Group -> failwith "Not implemented: group in sequence."
            | Sequence(_) -> failwith "Not implemented: sequence in sequence.")
        |> List.unzip
        |> (fun (a, b) -> a, b |> List.collect id)
    *)

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
            | PrimitiveType(_) as rtyp -> providedTy |> buildEnumerationType (spec, rtyp)
            | _ -> failwith "Simple types should not restrict complex types."
        | SimpleDefinition(ListDef) ->
            failwith "Not implemented: list in simpleType."
        | SimpleDefinition(Union(_)) ->
            failwith "Not implemented: union in simpleType."
        | ComplexDefinition(spec) ->
            // Abstract types will have only protected constructor.
            providedTy.AddMember(ProvidedConstructor([], invokeCode = (fun _ -> <@@ () @@>)))
            if spec.IsAbstract then
                providedTy.SetAttributes(providedTy.AttributesRaw ||| TypeAttributes.Abstract)
                annotationToText context spec.Annotation |> Option.iter providedTy.AddXmlDoc
            // Handle complex type content and add properties for attributes and elements.
            let specContent =
                match spec.Content with
                | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | PrimitiveType(_)
                    | ContentType as rtyp ->
                        let baseValueProp = providedTy |> addProperty("BaseValue", rtyp, false)
                        baseValueProp.AddCustomAttribute(Attributes.xrdElement None None None false true false)
                        Some(spec.Content)
                    | _ ->
                        failwith "ComplexType-s simpleContent should not extend complex types."
                | SimpleContent(SimpleContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s simpleContent."
                | ComplexContent(Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | ProvidedType(_) as baseTy ->
                        providedTy.SetBaseType(baseTy |> runtimeTypeToSystemType false)
                    | _ -> failwithf "Only complex types can be inherited! (%A)" spec.Base
                    Some(spec.Content)
                | ComplexContent(Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s complexContent"
                | Particle(spec) ->
                    Some(spec)
                | Empty ->
                    None
            specContent
            |> Option.fold (fun _ content -> providedTy |> addTypeProperties (collectComplexTypeContentProperties choiceNameGen seqNameGen context content)) ()
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
                | [ Choice(choice) ] ->
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
