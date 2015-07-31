module private XRoad.ProducerDefinition

open System
open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Serialization

open XRoad.CodeDom.Common
open XRoad.CodeDom.ServiceImpl
open XRoad.Common
open XRoad.ServiceDescription
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
          // Choice element specific attributes:
          ChoiceIdentifier: string option
          ChoiceElements: PropertyDefinition list
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
              ChoiceIdentifier = None
              ChoiceElements = []
              Documentation = doc }

    /// Build property declarations from property definitions and add them to owner type.
    let private addTypeProperties definitions ownerTy =
        definitions
        |> List.iter (fun definition ->
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
                    prop |> Prop.describe (Attributes.XmlArray(definition.IsNillable))
                         |> Prop.describe (Attributes.XmlArrayItem(itemName, definition.IsItemNillable.Value))
                         |> ignore
                | Some(true), _ ->
                    failwith "Wrapped array should match to CollectionType."
                | (None | Some(false)), _ ->
                    if definition.ChoiceIdentifier.IsNone then
                        prop |> Prop.describe (Attributes.XmlElement(elementName, definition.IsNillable))
                             |> ignore
                match definition.ChoiceIdentifier with
                | Some(identifierName) ->
                    prop |> Prop.describe (Attributes.XmlChoiceIdentifier(identifierName)) |> ignore
                    definition.ChoiceElements
                    |> List.iter (fun x -> prop |> Prop.describe (Attributes.XmlElement2(x.Name, x.Type.AsCodeTypeReference()))
                                                |> ignore)
                | None -> ()
            // Add extra types to owner type declaration.
            definition.AddedTypes |> List.iter (fun x -> ownerTy |> Cls.addMember x |> ignore))

    /// Create definition of property that accepts any element not defined in schema.
    let private buildAnyProperty () =
        let prop = PropertyDefinition.Create("AnyElements", false, None)
        { prop with Type = PrimitiveType(typeof<XmlElement[]>); IsAny = true }

    let private annotationToText (context: TypeBuilderContext) (annotation: Annotation option) =
        annotation
        |> Option.bind (fun annotation ->
            annotation.AppInfo
            |> List.collect (fun e -> e.Elements(xnsname "title" context.Protocol.Namespace) |> List.ofSeq)
            |> List.fold (fun doc el ->
                let lang = el |> attrOrDefault (xnsname "lang" XmlNamespace.Xml) "et"
                (lang, el.Value)::doc) []
            |> List.tryFind (fst >> ((=) context.LanguageCode))
            |> Option.map snd)

    /// Populate generated type declaration with properties specified in type schema definition.
    let rec build (context: TypeBuilderContext) runtimeType schemaType =
        // Extract type declaration from runtime type definition.
        let providedTy, providedTypeName =
            match runtimeType with
            | ProvidedType(decl, name) -> decl, name
            | _ -> failwith "Only generated types are accepted as arguments!"
        // Generates unique type name for every choice element.
        let choiceNameGenerator =
            let num = ref 0
            (fun () ->
                num := !num + 1
                sprintf "Choice%d" !num)
        // Parse schema definition and add all properties that are defined.
        match schemaType with
        | SimpleType(SimpleTypeSpec.Restriction(spec, annotation)) ->
            providedTy |> Code.comment (annotationToText context annotation) |> ignore
            match context.GetRuntimeType(SchemaType(spec.Base)) with
            | PrimitiveType(_) as rtyp ->
                providedTy |> addProperty("BaseValue", rtyp, false) |> Prop.describe Attributes.XmlText |> ignore
            | ContentType ->
                providedTy |> inheritBinaryContent |> ignore
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
                        providedTy |> addProperty("BaseValue", rtyp, false) |> Prop.describe Attributes.XmlText |> ignore
                        Some(spec.Content)
                    | _ ->
                        failwith "ComplexType-s simpleContent should not extend complex types."
                | SimpleContent(SimpleContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s simpleContent."
                | ComplexContent(ComplexContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | ProvidedType(baseDecl,_) as baseTy ->
                        providedTy |> Cls.setParent (baseTy.AsCodeTypeReference()) |> ignore
                        baseDecl |> Cls.describe (Attributes.XmlInclude(typeRefName providedTypeName)) |> ignore
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
                spec.Content
                |> List.map (fun item ->
                    match item with
                    | SequenceContent.Choice(cspec) ->
                        collectChoiceProperties choiceNameGenerator context cspec
                    | SequenceContent.Element(spec) ->
                        [ buildElementProperty context spec ]
                    | SequenceContent.Sequence(_) ->
                        failwith "Not implemented: sequence in complexType sequence."
                    | SequenceContent.Any ->
                        [ buildAnyProperty() ]
                    | SequenceContent.Group ->
                        failwith "Not implemented: group in complexType sequence.")
                |> List.collect (id)
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
            let subTy = Cls.create (name + "Type") |> Cls.addAttr TypeAttributes.Public
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
        match buildChoiceMembers spec context with
        | [] -> []
        | [ _ ] -> failwith "Not implemented: single option choice should be treated as regular sequence."
        | options ->
            // New unique name for choice properties and types.
            let choiceName = choiceNameGenerator()
            // Create enumeration type for options.
            let choiceEnum =
                Cls.createEnum (choiceName + "Type")
                |> Cls.setAttr TypeAttributes.Public
                |> Cls.describe Attributes.XmlTypeExclude
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

    /// Extract property definitions for all the elements defined in choice element.
    and private buildChoiceMembers (spec: ChoiceSpec) context =
        spec.Content
        |> List.map (
            function
            | ChoiceContent.Any ->
                failwith "Not implemented: any in choice."
            | ChoiceContent.Choice(_) ->
                failwith "Not implemented: choice in choice."
            | ChoiceContent.Element(espec) ->
                [ buildElementProperty context espec ]
            | ChoiceContent.Group ->
                failwith "Not implemented: group in choice."
            | ChoiceContent.Sequence(sspec) ->
                buildSequenceMembers context sspec)

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
                    (typ, Expr.instOf typ (tuple |> List.map (fun (varName,_) -> Expr.var varName)))
            | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
            | x::xs -> let inner = getReturnTypeTuple([x], xs)
                       let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_,typ) -> typ.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
                       (typ, Expr.instOf typ ((tuple |> List.map (fun (varName,_) -> Expr.var varName)) @ [snd inner]))
        let types =
            if isMultipart
            then ("reader.Context.Attachments", PrimitiveType(typeof<IDictionary<string,Stream>>))::types
            else types
        match types with
        | [] -> (CodeTypeReference(typeof<Void>), Expr.empty)
        | (varName, typ)::[] -> (typ.AsCodeTypeReference(), Expr.var varName)
        | many -> getReturnTypeTuple([], many)

    /// Generate types and serializing statements for root entities.
    /// Returns root name and type info.
    let private findRootNameAndType (context: TypeBuilderContext) (methodCall: MethodCall) (parameter: Parameter) =
        match methodCall, parameter.Type with
        | DocEncodedCall(_), SchemaName.SchemaType(_) // TODO: Encoding namespace for part name?
        | RpcEncodedCall(_), SchemaName.SchemaType(_) -> (Some(parameter.Name), None), parameter.Type
        | DocLiteralCall(_), SchemaName.SchemaType(_) -> (Some("Body"), Some(XmlNamespace.Soap)), parameter.Type
        | RpcLiteralCall(_), SchemaName.SchemaType(_) -> (Some(methodCall.Accessor.Value.LocalName), Some(methodCall.Accessor.Value.NamespaceName)), parameter.Type
        | DocLiteralCall(_), SchemaName.SchemaElement(elementName) ->
            match context.GetElementDefinition(context.GetElementSpec(elementName)) with
            | _, Definition(_) -> (None, None), (SchemaElement(elementName))
            | _, Name(name) -> (Some(elementName.LocalName), Some(elementName.NamespaceName)), SchemaType(name)
            | _ -> failwith "never"
        | RpcLiteralCall(_), SchemaName.SchemaElement(elementName) ->
            match context.GetElementDefinition(context.GetElementSpec(elementName)) with
            | _, Definition(_) -> (None, None), (SchemaElement(elementName))
            | _, Name(name) -> (Some(elementName.LocalName), Some(elementName.NamespaceName)), SchemaType(name)
            | _ -> failwith "never"
        | _ -> failwith "never"

    /// Initialize root attribute override for this method parameter.
    let initRoot varName name ns parentNs = seq {
        match name with
        | Some(v) ->
            yield Stmt.declVarWith<XmlRootAttribute> varName (Expr.inst<XmlRootAttribute> [Expr.value v])
            match ns with
            | Some(v) when v <> parentNs -> yield Stmt.assign (Expr.var varName @=> "Namespace") (Expr.value v)
            | _ -> ()
        | None ->
            yield Stmt.declVarWith<XmlRootAttribute> varName (Expr.inst<XmlRootAttribute> [Expr.nil])
        }

    /// Create expression which writes start of specified root element.
    let writeRootElementExpr rootName rootNamespace =
        let rootElementName =
            [rootName; rootName |> Option.bind (fun _ -> rootNamespace)]
            |> List.choose (id)
            |> List.map (Expr.value)
        (Expr.var "writer" @-> "WriteStartElement") @% rootElementName

    /// Create serializer code segment for given parameter.
    let private serializeParameter (context: TypeBuilderContext) (methodCall: MethodCall) serviceMethod parentNs (parameter: Parameter) =
        let serializerName = parameter.Name + "Serializer"
        let rootName = parameter.Name + "Root"
        // Initialize serializing
        let initSerializing schemaName (rnm, rns : string option) =
            // Add namespace definition if not equal to parent element namespace.
            match parentNs |> Option.orDefault "", rns with
            | v1, Some(v2) when v1 <> v2 ->
                serviceMethod |> Meth.addExpr ((!+ "writer" @-> "WriteAttributeString") @% [!^ "xmlns"; !^ "svcns"; Expr.nil; !^ v2]) |> ignore
            | _ -> ()
            // Add new argument to method for this message part.
            let runtimeType = context.GetRuntimeType(schemaName)
            serviceMethod |> Meth.addParamRef (runtimeType.AsCodeTypeReference()) parameter.Name |> ignore
            // Collection types need special handling for root element.
            match runtimeType with
            | CollectionType(itemType, itemName, _) ->
                // Write array wrapper manually and serialize each array item separately.
                serviceMethod
                |> Meth.addExpr (writeRootElementExpr rnm rns)
                |> Meth.addStmt (Stmt.condIfElse (Op.isNull (Expr.var parameter.Name))
                                                 [Stmt.ofExpr ((Expr.var "writer" @-> "WriteAttributeString") @% [Expr.value "nil"; Expr.value XmlNamespace.Xsi; Expr.value "true"])]
                                                 [Stmt.declVarWith<XmlRootAttribute> rootName (Expr.inst<XmlRootAttribute> [Expr.value itemName])
                                                  Stmt.forLoop (Stmt.declVarWith<int> "i" (Expr.value 0))
                                                               (Op.greater (Expr.var parameter.Name @=> "Length") (Expr.var "i"))
                                                               (Stmt.assign (Expr.var "i") (Op.plus (Expr.var "i") (Expr.value 1)))
                                                               [Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf (itemType.AsCodeTypeReference()); Expr.nil; Arr.createOfSize<Type> 0; Expr.var rootName; Expr.nil ])
                                                                Stmt.ofExpr ((Expr.var serializerName @-> "Serialize") @% [Expr.var "writer"; Expr.var parameter.Name @? (Expr.var "i")])]])
                |> Meth.addExpr ((Expr.var "writer" @-> "WriteEndElement") @% [])
                |> ignore
            | _ ->
                initRoot rootName rnm rns (parentNs |> Option.orDefault "")
                |> Seq.iter (fun s -> serviceMethod |> Meth.addStmt s |> ignore)
                serviceMethod
                |> Meth.addStmt (Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf (runtimeType.AsCodeTypeReference()); Expr.nil; Arr.createOfSize<Type> 0; Expr.var rootName; Expr.nil ]))
                |> Meth.addExpr ((Expr.var serializerName @-> "Serialize") @% [Expr.var "writer"; Expr.var parameter.Name])
                |> ignore
        let rootName, typeName = findRootNameAndType context methodCall parameter
        initSerializing typeName rootName

    /// Create deserializer code segment for given parameter.
    let private buildDeserialization (context: TypeBuilderContext) undescribedFaults (methodCall: MethodCall) parentNs : CodeTypeReference * CodeStatement list =
        let statements = List<CodeStatement>()
        let returnType = List<string * RuntimeType>()

        // Create separate deserializer calls for each output parameter.
        methodCall.Parameters
        |> List.iteri (fun i parameter ->
            let (rootName, rootNamespace), typeName = findRootNameAndType context methodCall parameter
            let serializerName = parameter.Name + "Serializer"
            let rootVarName = parameter.Name + "Root"
            let runtimeType = context.GetRuntimeType(typeName)
            returnType.Add(parameter.Name, runtimeType)
            let deserializerCallExpr =
                match runtimeType with
                | CollectionType(itemType, itemName, _) ->
                    let listType = CodeTypeReference("System.Collections.Generic.List", itemType.AsCodeTypeReference())
                    let nullVarName = sprintf "v%dNull" i
                    let listVarName = sprintf "v%dList" i
                    let typ = itemType.AsCodeTypeReference()
                    [ Stmt.declVarWith<string> nullVarName ((Expr.var "reader" @-> "GetAttribute") @% [Expr.value "nil"; Expr.value XmlNamespace.Xsi])
                      Stmt.condIfElse (Op.boolAnd (Op.isNotNull (Expr.var nullVarName))
                                                  (Op.equals ((((Expr.var nullVarName @-> "ToLower") @% []) @-> "Replace") @% [Expr.value "true"; Expr.value "1"])
                                                             (Expr.value "1")))
                                      [ Stmt.assign (Expr.var (sprintf "v%d" i)) Expr.nil ]
                                      [ Stmt.declVarRefWith listType listVarName (Expr.instOf listType [])
                                        Stmt.declVarWith<XmlRootAttribute> rootVarName (Expr.inst<XmlRootAttribute> [Expr.value itemName])
                                        Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf typ; Expr.nil; Arr.createOfSize<Type> 0; Expr.var rootVarName; Expr.nil])
                                        Stmt.whileLoop (Expr.var "MoveToElement" @%% [Expr.var "reader"; Expr.nil; Expr.nil; Expr.value 4])
                                                       [Stmt.ofExpr ((Expr.var listVarName @-> "Add") @% [Expr.cast typ ((Expr.var serializerName @-> "Deserialize") @% [Expr.var "reader"])])]
                                        Stmt.assign (Expr.var (sprintf "v%d" i)) ((Expr.var listVarName @-> "ToArray") @% []) ] ]
                | _ ->
                    let typ = runtimeType.AsCodeTypeReference()
                    statements.AddRange(initRoot rootVarName rootName rootNamespace (parentNs |> Option.orDefault ""))
                    [ Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf typ; Expr.nil; Arr.createOfSize<Type> 0; Expr.var rootVarName; Expr.nil])
                      Stmt.assign (Expr.var (sprintf "v%d" i)) (Expr.cast typ ((Expr.var serializerName @-> "Deserialize") @% [Expr.var "reader"])) ]
            let deserializeExpr =
                if parameter.Name = "keha" && undescribedFaults then
                    [ Stmt.ofExpr((Expr.var "reader" @-> "SetBookmark") @% [Expr.value "keha"])
                      Stmt.condIfElse (Expr.var "MoveToElement" @%% [Expr.var "reader"; Expr.value "faultCode"; Expr.value ""; Expr.value 4])
                                      [ Stmt.ofExpr ((Expr.var "reader" @-> "ReturnToAndRemoveBookmark") @% [Expr.value "keha"])
                                        Stmt.throw<Exception> [(Expr.var "reader" @-> "ReadInnerXml") @% []] ]
                                      (Stmt.ofExpr ((Expr.var "reader" @-> "ReturnToAndRemoveBookmark") @% [Expr.value "keha"]) :: deserializerCallExpr) ]
                else deserializerCallExpr
            statements.Add(Stmt.condIf (Op.equals (Expr.var "reader" @=> "LocalName")
                                                    (Expr.value parameter.Name))
                                        deserializeExpr))

        // Build return type and expression which returns the value.
        let retType, returnExpr = makeReturnType methodCall.IsMultipart (returnType |> Seq.mapi (fun i (_,rt) -> (sprintf "v%d" i), rt) |> List.ofSeq)

        // Build body part of deserialization block.
        let bodyStatements = List<_>()
        bodyStatements.AddRange(returnType |> Seq.mapi (fun i (_,runtimeType) -> Stmt.declVarRefWith (runtimeType.AsCodeTypeReference()) (sprintf "v%d" i) Expr.nil))
        bodyStatements.Add(Stmt.whileLoop (Expr.var "MoveToElement" @%% [Expr.var "reader"; Expr.nil; Expr.nil; Expr.value 3]) (statements |> List.ofSeq))
        bodyStatements.Add(Stmt.ret returnExpr)

        retType, bodyStatements |> List.ofSeq

    /// Build content for each individual service call method.
    let build context undescribedFaults (operation: ServicePortMethod) =
        let serviceMethod =
            Meth.create operation.Name
            |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
            |> Code.comment operation.Documentation

        let requiredHeadersExpr = operation.InputParameters.RequiredHeaders |> List.map (!^) |> Arr.create<string>
        let serviceName = match operation.Version with Some v -> sprintf "%s.%s" operation.Name v | _ -> operation.Name

        // CodeDom doesn't support delegates, so we have to improvise.
        serviceMethod
        |> Meth.addStmt (Stmt.declVarWith<string[]> "requiredHeaders" requiredHeadersExpr)
        |> Meth.addStmt (Stmt.declVarWith<Action<XmlWriter>> "writeHeader" (Expr.code "(writer) => { //"))
        |> Meth.addExpr ((Expr.parent @-> "WriteHeader") @% [Expr.var "writer"; Expr.value serviceName; Expr.var "requiredHeaders"])
        |> Meth.addExpr (Expr.code "}")
        |> Meth.addStmt (Stmt.declVarRefWith (CodeTypeReference("System.Action", typeRef<XRoad.XRoadXmlWriter>)) "writeBody" (Expr.code "(writer) => { //"))
        |> iif (operation.InputParameters.IsMultipart) (fun x -> x |> Meth.addStmt (Stmt.assign ((Expr.var "writer" @=> "Context") @=> "IsMultipart") (Expr.value true)))
        |> ignore

        // Write SOAP Body element unless it is defined as parameter value type.
        let writeSoapBody =
            match operation.InputParameters with
            | DocLiteralCall(wr) ->
                match wr.Parameters with
                | [p] ->
                    match p.Type with
                    | SchemaName.SchemaType(_) -> false
                    | SchemaName.SchemaElement(_) -> true
                | _ -> true
            | _ -> true
        if writeSoapBody then
            serviceMethod
            |> Meth.addExpr ((!+ "writer" @-> "WriteStartElement") @% [!^ "Body"; !^ XmlNamespace.SoapEnv])
            |> iif (operation.InputParameters.Accessor.IsSome) (fun m -> m |> Meth.addExpr ((!+ "writer" @-> "WriteAttributeString") @% [!^ "xmlns"; !^ "acc"; Expr.nil; !^ operation.InputParameters.Accessor.Value.NamespaceName]))
            |> ignore

        // Write accessor element unless it is defined as parameter value type or method call uses document style.
        let writeAccessorElement =
            match operation.InputParameters with
            | RpcLiteralCall(_,w) ->
                match w.Parameters with
                | [p] ->
                    match p.Type with
                    | SchemaName.SchemaType(_) -> false
                    | SchemaName.SchemaElement(_) -> true
                | _ -> true
            | RpcEncodedCall(_) -> true
            | _ -> false
        if writeAccessorElement then
            match operation.InputParameters.Accessor with
            | Some(acc) -> serviceMethod |> Meth.addExpr ((!+ "writer" @-> "WriteStartElement") @% [!^ acc.LocalName; !^ acc.NamespaceName]) |> ignore
            | None -> ()

        // Create separate serializer for each parameter.
        let parentNs = if writeAccessorElement then operation.InputParameters.Accessor |> Option.map (fun x -> x.NamespaceName)
                       elif writeSoapBody then Some(XmlNamespace.Soap)
                       else None
        operation.InputParameters.Parameters |> List.iter (serializeParameter context operation.InputParameters serviceMethod parentNs)

        // Finish body writer delegate.
        serviceMethod
        |> iif (writeAccessorElement) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteEndElement") @% []))
        |> iif (writeSoapBody) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteEndElement") @% []))
        |> Meth.addExpr (Expr.code "}")
        |> ignore

        // Read accessor element unless it is defined as parameter value type or method call uses document style.
        let readAccessorElement =
            match operation.OutputParameters with
            | RpcLiteralCall(_,w) ->
                match w.Parameters with
                | [p] ->
                    match p.Type with
                    | SchemaName.SchemaType(_) -> false
                    | SchemaName.SchemaElement(_) -> true
                | _ -> true
            | RpcEncodedCall(_) -> true
            | _ -> false

        // Build output parameters deserialization expressions.
        let parentNs = if readAccessorElement then operation.OutputParameters.Accessor |> Option.map (fun x -> x.NamespaceName)
                       //elif writeSoapBody then Some(XmlNamespace.Soap)
                       else Some(XmlNamespace.Soap)
        let returnType, deserializeExpr = buildDeserialization context undescribedFaults operation.OutputParameters parentNs

        // Reading body of response.
        serviceMethod
        |> Meth.returnsOf returnType
        |> Meth.addStmt (Stmt.declVarRefWith (CodeTypeReference("System.Func", typeRef<XmlReader>, returnType)) "readBody" (Expr.code "(r) => { //"))
        |> Meth.addStmt (if undescribedFaults
                         then Stmt.declVarRefWith (typeRefName "XmlBookmarkReader") "reader" (Expr.cast (typeRefName "XmlBookmarkReader") (Expr.var "r"))
                         else Stmt.declVarRefWith (typeRef<XRoad.XRoadXmlReader>) "reader" (Expr.cast (typeRef<XRoad.XRoadXmlReader>) (Expr.var "r")))
        |> iif readAccessorElement (fun x ->
            x |> Meth.addStmt (Stmt.condIf (Op.boolOr (Op.notEquals (Expr.var "reader" @=> "LocalName")
                                                                    (Expr.value operation.OutputParameters.Accessor.Value.LocalName))
                                                      (Op.notEquals (Expr.var "reader" @=> "NamespaceURI")
                                                                    (Expr.value operation.OutputParameters.Accessor.Value.NamespaceName)))
                                           [Stmt.throw<Exception> [Expr.value "Invalid response message."]]))
        |> ignore

        // Deserialize main content.
        deserializeExpr |> List.iter (fun s -> serviceMethod |> Meth.addStmt s |> ignore)

        // Finish delegate block.
        serviceMethod |> Meth.addExpr (Expr.code "}") |> ignore

        // Multipart services have separate argument: list of MIME/multipart attachments.
        let attachmentsExpr =
            if operation.InputParameters.IsMultipart then
                serviceMethod |> Meth.addOptParam<IDictionary<string,Stream>> "attachments" |> ignore
                Expr.var "attachments"
            else Expr.nil

        // Execute base class service call method with custom serialization delegates.
        let methodCall = ((Expr.parent @-> "MakeServiceCall") @<> [returnType]) @% [!^ operation.InputParameters.IsEncoded; attachmentsExpr; Expr.var "writeHeader"; Expr.var "writeBody"; Expr.var "readBody"]

        // Make return statement if definition specifies result.
        let isEmptyResponse = (not operation.OutputParameters.IsMultipart) && (operation.OutputParameters.Parameters |> List.isEmpty)
        serviceMethod |> Meth.addStmt (if isEmptyResponse then Stmt.ofExpr methodCall else Stmt.ret methodCall)

/// Builds all types, namespaces and services for give producer definition.
/// Called by type provider to retrieve assembly details for generated types.
let makeProducerType (typeNamePath: string [], producerUri, undescribedFaults, languageCode) =
    // Load schema details from specified file or network location.
    let schema = ProducerDescription.Load(resolveUri producerUri, languageCode)

    // Initialize type and schema element lookup context.
    let context = TypeBuilderContext.FromSchema(schema, languageCode)

    // Create base type which provides access to service calls.
    let portBaseTy = makeServicePortBaseType undescribedFaults context.Protocol

    // Create base type which holds types generated from all provided schema-s.
    let serviceTypesTy = Cls.create "DefinedTypes" |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

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
        // Undescribed faults require looser navigation in XmlReader.
        |> iif undescribedFaults (fun x -> x |> Cls.addMember (createTypeFromAssemblyResource("XmlBookmarkReader.cs")))
        |> iif (not undescribedFaults) (fun x -> x |> Cls.addMember (Cls.create "XmlBookmarkReader"
                                                                     |> Cls.setParent(typeRef<XmlTextReader>)
                                                                     |> Cls.addMember (Prop.createRef (typeRef<XRoad.XRoadXmlReader>) "Reader"
                                                                                       |> Prop.addGetStmt (Stmt.ret Expr.nil)
                                                                                       |> Prop.setAttr (MemberAttributes.Public ||| MemberAttributes.Final))))
        |> Cls.addMember portBaseTy
        |> Cls.addMember serviceTypesTy

    // Create methods for all operation bindings.
    schema.Services
    |> List.iter (fun service ->
        let serviceTy = Cls.create service.Name |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic
        service.Ports
        |> List.iter (fun port ->
            let ctor =
                Ctor.create()
                |> Ctor.setAttr MemberAttributes.Public
                |> Ctor.addBaseArg (Expr.value port.Uri)
                |> Ctor.addBaseArg (Expr.value port.Producer)
            let portTy =
                Cls.create port.Name
                |> Cls.setAttr TypeAttributes.Public
                |> Cls.setParent (typeRefName portBaseTy.Name)
                |> Cls.addMember ctor
                |> Code.comment port.Documentation
            serviceTy |> Cls.addMember portTy |> ignore
            port.Methods
            |> List.iter (fun op -> portTy |> Cls.addMember (ServiceBuilder.build context undescribedFaults op) |> ignore)
            )
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
