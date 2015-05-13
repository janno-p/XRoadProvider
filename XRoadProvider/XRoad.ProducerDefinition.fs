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
          ChoiceElements: PropertyDefinition list }
        /// Initializes default property with name and optional value.
        static member Create(name, isOptional) =
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
              ChoiceElements = [] }

    /// Build property declarations from property definitions and add them to owner type.
    let private addTypeProperties definitions ownerTy =
        definitions
        |> List.iter (fun definition ->
            // Most of the conditions handle XmlSerializer specific attributes.
            let prop = ownerTy |> addProperty(definition.Name, definition.Type, definition.IsOptional)
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
                        prop |> Prop.describe (Attributes.XmlElement(definition.IsNillable))
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
        let prop = PropertyDefinition.Create("AnyElements", false)
        { prop with Type = PrimitiveType(typeof<XmlElement[]>); IsAny = true }

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
        | SimpleType(SimpleTypeSpec.Restriction(spec)) ->
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
                           |> ignore
            // Handle complex type content and add properties for attributes and elements.
            let specContent =
                match spec.Content with
                | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | PrimitiveType(_)
                    | ContentType as rtyp ->
                        providedTy |> addProperty("BaseValue", rtyp, false) |> Prop.describe Attributes.XmlText |> ignore
                        spec.Content
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
                    spec.Content
                | ComplexContent(ComplexContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s complexContent"
                | ComplexTypeContent.Particle(spec) ->
                    spec
            providedTy |> addTypeProperties (collectComplexTypeContentProperties choiceNameGenerator context specContent)
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
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
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
        buildPropertyDef schemaType spec.MaxOccurs name spec.IsNillable (spec.MinOccurs = 0u) context

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
        let prop = buildPropertyDef schemaType 1u name false isOptional context
        { prop with IsAttribute = true }

    /// Build default property definition from provided schema information.
    and private buildPropertyDef schemaType maxOccurs name isNillable isOptional context =
        let propertyDef = PropertyDefinition.Create(name, isOptional)
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
                let prop = PropertyDefinition.Create(choiceName + "Name", false)
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
                let prop = PropertyDefinition.Create(choiceName + (if isArray then "Items" else"Item"), false)
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
    let private makeReturnType (types: RuntimeType list) =
        let rec getReturnTypeTuple (tuple: (int * RuntimeType) list, types) =
            match types with
            | [] -> let typ = CodeTypeReference("System.Tuple", tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference()) |> Array.ofList)
                    (typ, Expr.instOf typ (tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i))))
            | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
            | x::xs -> let inner = getReturnTypeTuple([x], xs)
                       let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
                       (typ, Expr.instOf typ ((tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i))) @ [snd inner]))
        match types |> List.mapi (fun i x -> (i, x)) with
        | [] -> (CodeTypeReference(typeof<Void>), Expr.empty)
        | (i,tp)::[] -> (tp.AsCodeTypeReference(), Expr.var(sprintf "v%d" i))
        | many -> getReturnTypeTuple([], many)

    /// Create root element for service request. Root elements are feeded to XmlSerializer as arguments,
    /// so they need to define XmlRoot attributes to override default element name (type name).
    /// Depending on operation style and MIME/multipart mode, some overrides are required to default serialization.
    let private buildParameterType (context: TypeBuilderContext) isMultipart (part: MessagePart) =
        // Enables MIME/multipart serialization mode via overrides:
        let enableOverrides varName = seq {
            if isMultipart then
                yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") (Expr.inst<XmlAttributeOverrides> [])
                yield Stmt.declVarWith<XmlAttributes> (varName + "Value") (Expr.inst<XmlAttributes> [])
                yield Stmt.assign (Expr.var (varName + "Value") @=> "XmlIgnore") (Expr.value true)
                yield Stmt.ofExpr ((Expr.var (varName + "Overrides") @-> "Add") @% [Expr.typeOf (CodeTypeReference("BinaryContent")); Expr.value "Value"; Expr.var (varName + "Value")])
            else
                yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") Expr.nil
        }
        // Generate types for root entities:
        match context.Style, part.SchemaEntity with
        | DocLiteral, SchemaType(t) ->
            failwithf "Document/Literal style message part '%s' should reference global element as message part, but type '%s' is used instead" part.Name t.LocalName
        | RpcEncoded, SchemaElement(e) ->
            failwithf "RPC/Encoded style message part '%s' should reference global type as message part, but element '%s' is used instead" part.Name e.LocalName
        | DocLiteral, SchemaElement(elementName) ->
            // Document/literal style references `element` objects in message parts.
            // Find element type definition:
            let elemType = snd <| context.GetElementDefinition(context.GetElementSpec(elementName))
            match elemType with
            | Name(name) ->
                // Type is defined by element type; root element has element name and namespace defined in operation binding.
                context.GetRuntimeType(SchemaType(name)),
                fun varName ->
                    [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") (Expr.inst<XmlRootAttribute> [Expr.value elementName.LocalName])
                      yield Stmt.assign (Expr.var (varName + "Root") @=> "Namespace") (Expr.value elementName.NamespaceName)
                      yield! enableOverrides varName ]
            | Reference(_) ->
                failwith "Not implemented: message part should use elements defined in target namespace."
            | Definition(_) ->
                // Element itself is used as root element type, so no overrides are neccessary.
                context.GetRuntimeType(SchemaElement(elementName)),
                fun varName ->
                    [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") Expr.nil
                      yield! enableOverrides varName ]
        | RpcEncoded, SchemaType(typeName) ->
            // Referenced type is also root element type; root element name matches part name.
            context.GetRuntimeType(SchemaType(typeName)), 
            fun varName ->
                [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") (Expr.inst<XmlRootAttribute> [Expr.value part.Name])
                  yield! enableOverrides varName ]

    /// Build content for each individual service call method.
    let build context undescribedFaults operation =
        let isMultipart = operation.Request.MultipartContent |> List.isEmpty |> not
        let serviceMethod = Meth.create operation.Name.LocalName |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        let requestParameters = operation.Request.Body.Parts |> List.map (fun p -> p |> buildParameterType context isMultipart, p.Name)
        let responseParameters = operation.Response.Body.Parts |> List.map (fun p -> p |> buildParameterType context isMultipart, p.Name)
        let returnType, returnExpr = responseParameters |> List.map (fst >> fst) |> makeReturnType

        serviceMethod.ReturnType <- returnType

        let requiredHeadersExpr = operation.GetRequiredHeaders() |> List.map Expr.value |> Arr.create<string>
        let serviceName = match operation.Version with Some v -> sprintf "%s.%s" operation.Name.LocalName v | _ -> operation.Name.LocalName

        // CodeDom doesn't support delegates, so we have to improvise.
        serviceMethod
        |> Meth.addStmt (Stmt.declVarWith<string[]> "requiredHeaders" requiredHeadersExpr)
        |> Meth.addStmt (Stmt.declVarWith<Action<XmlWriter>> "writeHeader" (Expr.code "(writer) => { //"))
        |> Meth.addExpr ((Expr.parent @-> "WriteHeader") @% [Expr.var "writer"; Expr.value serviceName; Expr.var "requiredHeaders"])
        |> Meth.addExpr (Expr.code "}")
        |> Meth.addStmt (Stmt.declVarWith<Action<XmlWriter>> "writeBody" (Expr.code "(writer) => { //"))
        |> Meth.addExpr ((Expr.var "writer" @-> "WriteAttributeString") @% [Expr.value "xmlns"; Expr.value "svc"; Expr.nil; Expr.value operation.Name.NamespaceName])
        |> iif (operation.Request.Body.Namespace <> operation.Name.NamespaceName) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteAttributeString") @% [Expr.value "xmlns"; Expr.value "svcns"; Expr.nil; Expr.value operation.Request.Body.Namespace]))
        |> iif (operation.Style = RpcEncoded) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteStartElement") @% [Expr.value operation.Request.Name.LocalName; Expr.value operation.Request.Body.Namespace]))
        |> ignore

        // Create separate serializer for each parameter.
        requestParameters
        |> List.iter (fun ((runtimeType, overrideFunc), partName) ->
            let serializerName = partName + "Serializer"
            let typ = runtimeType.AsCodeTypeReference()
            serviceMethod |> Meth.addParamRef typ partName |> ignore
            partName |> overrideFunc |> List.iter (fun s -> serviceMethod |> Meth.addStmt s |> ignore)
            serviceMethod
            |> Meth.addStmt (Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf typ; Expr.var (partName + "Overrides"); Arr.createOfSize<Type> 0; Expr.var (partName + "Root"); Expr.nil ]))
            |> Meth.addExpr ((Expr.var serializerName @-> "Serialize") @% [Expr.var "writer"; Expr.var partName])
            |> ignore)

        // Create separate deserializer call for each output parameter.
        let deserializePartsExpr =
            responseParameters
            |> List.mapi (fun i ((runtimeType, overrideFunc), partName) ->
                let serializerName = partName + "Serializer"
                let typ = runtimeType.AsCodeTypeReference()
                let deserializeExpr =
                    (overrideFunc partName) @
                        [ Stmt.declVarWith<XmlSerializer> serializerName (Expr.inst<XmlSerializer> [Expr.typeOf typ; Expr.var (partName + "Overrides"); Arr.createOfSize<Type> 0; Expr.var (partName + "Root"); Expr.nil])
                          Stmt.assign (Expr.var(sprintf "v%d" i)) (Expr.cast typ ((Expr.var serializerName @-> "Deserialize") @% [Expr.var "reader"])) ]
                let deserializeExpr =
                    if partName = "keha" && undescribedFaults then
                      [ (Expr.var "reader" @-> "SetBookmark") @% [Expr.value "keha"] |> Stmt.ofExpr
                        Stmt.condIfElse ((Expr.this @-> "MoveToElement") @% [Expr.var "reader"; Expr.value "faultCode"; Expr.value ""; Expr.value 4])
                                        [ (Expr.var "reader" @-> "ReturnToAndRemoveBookmark") @% [Expr.value "keha"] |> Stmt.ofExpr
                                          Stmt.throw<Exception> [(Expr.var "reader" @-> "ReadInnerXml") @% []] ]
                                        ([ (Expr.var "reader" @-> "ReturnToAndRemoveBookmark") @% [Expr.value "keha"] |> Stmt.ofExpr ] @ deserializeExpr) ]
                    else deserializeExpr
                Stmt.condIf (Op.equals (Expr.var "reader" @=> "LocalName")
                                       (Expr.value partName))
                            deserializeExpr)

        // Finish body writer delegate.
        serviceMethod
        |> iif (operation.Style = RpcEncoded) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteEndElement") @% []))
        |> Meth.addExpr (Expr.code "}")
        // Reading body of response.
        |> Meth.addStmt (Stmt.declVarRefWith (CodeTypeReference("System.Func", typeRef<XmlReader>, returnType)) "readBody" (Expr.code "(r) => { //"))
        |> Meth.addStmt (if undescribedFaults
                         then Stmt.declVarRefWith (typeRefName "XmlBookmarkReader") "reader" (Expr.cast (typeRefName "XmlBookmarkReader") (Expr.var "r"))
                         else Stmt.declVarWith<XmlReader> "reader" (Expr.var "r"))
        |> Meth.addStmt (Stmt.condIf (Op.boolOr (Op.notEquals (Expr.var "reader" @=> "LocalName")
                                                              (Expr.value operation.Response.Name.LocalName))
                                                (Op.notEquals (Expr.var "reader" @=> "NamespaceURI")
                                                              (Expr.value operation.Response.Body.Namespace)))
                                     [Stmt.throw<Exception> [Expr.value "Invalid response message."]])
        |> ignore

        responseParameters
        |> List.iteri (fun i ((runtimeType,_),_) ->
            let typ = runtimeType.AsCodeTypeReference()
            serviceMethod |> Meth.addStmt (Stmt.declVarRefWith typ (sprintf "v%d" i) Expr.nil) |> ignore)

        serviceMethod
        |> Meth.addStmt (Stmt.whileLoop ((Expr.this @-> "MoveToElement") @% [Expr.var "reader"; Expr.nil; Expr.nil; Expr.value 3]) deserializePartsExpr)
        |> Meth.addStmt (Stmt.ret returnExpr)
        |> Meth.addExpr (Expr.code "}")
        |> ignore

        // Multipart services have separate argument: list of MIME/multipart attachments.
        let attachmentsExpr =
            if isMultipart then
                serviceMethod |> Meth.addParam<IDictionary<string,Stream>> "attachments" |> ignore
                Expr.var "attachments"
            else Expr.nil

        match operation.Documentation.TryGetValue("et") with
        | true, doc -> serviceMethod.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
        | _ -> ()

        // Execute base class service call method with custom serialization delegates.
        let methodCall = ((Expr.parent @-> "MakeServiceCall") @<> [returnType]) @% [attachmentsExpr; Expr.var "writeHeader"; Expr.var "writeBody"; Expr.var "readBody"]

        if responseParameters.IsEmpty then serviceMethod |> Meth.addExpr methodCall |> ignore
        else serviceMethod |> Meth.addStmt (Stmt.ret methodCall) |> ignore 

        serviceMethod

/// Builds all types, namespaces and services for give producer definition.
/// Called by type provider to retrieve assembly details for generated types.
let makeProducerType (typeNamePath: string [], producerUri, undescribedFaults) =
    // Load schema details from specified file or network location.
    let schema = ProducerDescription.Load(resolveUri producerUri)

    // Initialize type and schema element lookup context.
    let context = TypeBuilderContext.FromSchema(schema)

    // Create base type which provides access to service calls.
    let portBaseTy = makeServicePortBaseType undescribedFaults context.Style

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
        |> iif undescribedFaults (fun x -> x |> Cls.addMember (createXmlBookmarkReaderType()))
        |> Cls.addMember portBaseTy
        |> Cls.addMember serviceTypesTy
        |> Cls.addMember (createBinaryContentType())

    // Create methods for all operation bindings.
    schema.Services
    |> List.iter (fun service ->
        let serviceTy =
            Cls.create service.Name
            |> Cls.setAttr TypeAttributes.Public
            |> Cls.asStatic
        service.Ports
        |> List.iter (fun port ->
            let ctor =
                Ctor.create()
                |> Ctor.setAttr MemberAttributes.Public
                |> Ctor.addBaseArg (Expr.value port.Address)
                |> Ctor.addBaseArg (Expr.value port.Producer)
            let portTy =
                Cls.create port.Name
                |> Cls.setAttr TypeAttributes.Public
                |> Cls.setParent (typeRefName portBaseTy.Name)
                |> Cls.addMember ctor
            serviceTy |> Cls.addMember portTy |> ignore
            match port.Documentation.TryGetValue("et") with
            | true, doc -> portTy.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
            | _ -> ()
            port.Operations
            |> List.iter (fun op ->
                portTy
                |> Cls.addMember (ServiceBuilder.build context undescribedFaults op)
                |> ignore))
        targetClass |> Cls.addMember serviceTy |> ignore)

    // Create types for all type namespaces.
    context.CachedNamespaces |> Seq.iter (fun kvp -> kvp.Value |> serviceTypesTy.Members.Add |> ignore)

    // Initialize default namespace to hold main type.
    let codeNamespace = CodeNamespace(String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1)))
    codeNamespace.Types.Add(targetClass) |> ignore

    // Compile the assembly and return to type provider.
    let assembly = Compiler.buildAssembly(codeNamespace)
    assembly.GetType(sprintf "%s.%s" codeNamespace.Name targetClass.Name)
