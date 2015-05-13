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

let private getRequiredHeaders(operation: Operation) =
    let headers, rest =
        operation.Request.Header
        |> List.partition (fun part ->
            match part.SchemaEntity with
            | SchemaElement(XteeHeader(_)) when operation.Style = RpcEncoded -> true
            | SchemaElement(XRoadHeader(_)) when operation.Style = DocLiteral -> true
            | _ -> false)
    if rest.Length > 0 then
        failwithf "Unhandled SOAP Header elements detected: %A" rest
    headers |> List.map (fun part -> part.Name)

let makeReturnType (types: RuntimeType list) =
    let rec getReturnTypeTuple (tuple: (int * RuntimeType) list, types) =
        match types with
        | [] -> let typ = CodeTypeReference("System.Tuple", tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference()) |> Array.ofList)
                (typ, Expr.instOf typ (tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i))))
        | x::xs when tuple.Length < 7 -> getReturnTypeTuple(x :: tuple, xs)
        | x::xs -> let inner = getReturnTypeTuple([x], xs)
                   let typ = CodeTypeReference("System.Tuple", ((tuple |> List.map (fun (_, x) -> x.AsCodeTypeReference())) @ [fst inner]) |> Array.ofList)
                   (typ, Expr.instOf typ ((tuple |> List.map (fun (i, _) -> Expr.var(sprintf "v%d" i))) @ [snd inner]))
    match types |> List.mapi (fun i x -> (i, x)) with
    | [] -> (CodeTypeReference(typeof<Void>), Expr.var("???"))
    | (i,tp)::[] -> (tp.AsCodeTypeReference(), Expr.var(sprintf "v%d" i))
    | many -> getReturnTypeTuple([], many)

let buildParameterType (context: TypeBuilderContext) isMultipart (part: MessagePart) =
    match context.Style, part.SchemaEntity with
    | DocLiteral, SchemaType(t) ->
        failwithf "Document/Literal style message part '%s' should reference global element as message part, but type '%s' is used instead" part.Name t.LocalName
    | RpcEncoded, SchemaElement(e) ->
        failwithf "RPC/Encoded style message part '%s' should reference global type as message part, but element '%s' is used instead" part.Name e.LocalName
    | DocLiteral, SchemaElement(elementName) ->
        let elemType = snd <| context.GetElementDefinition(context.GetElementSpec(elementName))
        match elemType with
        | Name(name) ->
            context.GetRuntimeType(SchemaType(name)),
            fun varName ->
                [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") (Expr.inst<XmlRootAttribute> [Expr.value elementName.LocalName])
                  yield Stmt.assign (Expr.var (varName + "Root") @=> "Namespace") (Expr.value elementName.NamespaceName)
                  if isMultipart then
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") (Expr.inst<XmlAttributeOverrides> [])
                    yield Stmt.declVarWith<XmlAttributes> (varName + "Value") (Expr.inst<XmlAttributes> [])
                    yield Stmt.assign (Expr.var (varName + "Value") @=> "XmlIgnore") (Expr.value true)
                    yield Stmt.ofExpr ((Expr.var (varName + "Overrides") @-> "Add") @% [Expr.typeOf (CodeTypeReference("BinaryContent")); Expr.value "Value"; Expr.var (varName + "Value")])
                  else
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") Expr.nil ]
        | Reference(_) ->
            failwith "not implemented"
        | Definition(_) ->
            context.GetRuntimeType(SchemaElement(elementName)),
            fun varName ->
                [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") Expr.nil
                  if isMultipart then
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") (Expr.inst<XmlAttributeOverrides> [])
                    yield Stmt.declVarWith<XmlAttributes> (varName + "Value") (Expr.inst<XmlAttributes> [])
                    yield Stmt.assign (Expr.var (varName + "Value") @=> "XmlIgnore") (Expr.value true)
                    yield Stmt.ofExpr ((Expr.var (varName + "Overrides") @-> "Add") @% [Expr.typeOf (CodeTypeReference("BinaryContent")); Expr.value "Value"; Expr.var (varName + "Value")])
                  else
                    yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") Expr.nil ]
    | RpcEncoded, SchemaType(typeName) ->
        context.GetRuntimeType(SchemaType(typeName)), 
        fun varName ->
            [ yield Stmt.declVarWith<XmlRootAttribute> (varName + "Root") (Expr.inst<XmlRootAttribute> [Expr.value part.Name])
              if isMultipart then
                yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") (Expr.inst<XmlAttributeOverrides> [])
                yield Stmt.declVarWith<XmlAttributes> (varName + "Value") (Expr.inst<XmlAttributes> [])
                yield Stmt.assign (Expr.var (varName + "Value") @=> "XmlIgnore") (Expr.value true)
                yield Stmt.ofExpr ((Expr.var (varName + "Overrides") @-> "Add") @% [Expr.typeOf (CodeTypeReference("BinaryContent")); Expr.value "Value"; Expr.var (varName + "Value")])
              else
                yield Stmt.declVarWith<XmlAttributeOverrides> (varName + "Overrides") Expr.nil ]

type PropertyDefinition =
    { Type: RuntimeType
      IsNillable: bool // Attributes.XmlElement(propdef.IsNillable)
      IsItemNillable: bool option
      AddedTypes: CodeTypeDeclaration list
      IsOptional: bool
      IsWrappedArray: bool option
      Name: string
      IsAttribute: bool
      IsAny: bool
      IsIgnored: bool
      ChoiceIdentifier: string option
      ChoiceElements: PropertyDefinition list }
    static member Create(name, isOptional) =
        { Type = RuntimeType.PrimitiveType(typeof<System.Void>) // ok
          IsNillable = false                                    // ok
          IsItemNillable = None                                 // ok
          AddedTypes = []                                       // ok
          IsOptional = isOptional                               // ok
          IsWrappedArray = None                                 // ok
          Name = name                                           // ok
          IsAttribute = false                                   // ok
          IsAny = false                                         // ok
          IsIgnored = false                                     // ok
          ChoiceIdentifier = None                               // ok
          ChoiceElements = []                                   // ok
          }

let private addTypeProperties definitions ownerTy =
    definitions
    |> List.iter (fun definition ->
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
        definition.AddedTypes |> List.iter (fun x -> ownerTy |> Cls.addMember x |> ignore))

let private buildAnyProperty () =
    let prop = PropertyDefinition.Create("AnyElements", false)
    { prop with Type = PrimitiveType(typeof<XmlElement[]>); IsAny = true }

/// Populate generated type declaration with properties specified in type schema definition.
let rec private buildType (context: TypeBuilderContext) runtimeType schemaType =
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

and private collectComplexTypeContentProperties choiceNameGenerator context spec =
    let attributeProperties = spec.Attributes |> List.map (buildAttributeProperty context)
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

and private buildElementProperty (context: TypeBuilderContext) (spec: ElementSpec) =
    let name, schemaType = context.GetElementDefinition(spec)
    buildPropertyDef schemaType spec.MaxOccurs name spec.IsNillable (spec.MinOccurs = 0u) context

and private buildAttributeProperty (context: TypeBuilderContext) (spec: AttributeSpec) =
    let name, schemaObject = context.GetAttributeDefinition(spec)
    let schemaType =
        match schemaObject with
        | Definition(simpleTypeSpec) -> Definition(SimpleType(simpleTypeSpec))
        | Name(name) -> Name(name)
        | Reference(ref) -> Reference(ref)
    let isOptional = match spec.Use with Required -> true | _ -> false
    let prop = buildPropertyDef schemaType 1u name false isOptional context
    { prop with IsAttribute = true }

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
            buildType context runtimeType def
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
        buildType context runtimeType def
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

and private collectChoiceProperties choiceNameGenerator context spec : PropertyDefinition list =
    match buildChoiceMembers spec context with
    | [] -> []
    | [ _ ] -> failwith "Not implemented: single option choice should be treated as regular sequence."
    | options ->
        let choiceName = choiceNameGenerator()
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

let makeProducerType (typeNamePath: string [], producerUri, undescribedFaults) =
    let schema = ProducerDescription.Load(resolveUri producerUri)
    let context = TypeBuilderContext.FromSchema(schema)

    let portBaseTy = makeServicePortBaseType undescribedFaults context.Style
    let serviceTypesTy = Cls.create "DefinedTypes" |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

    let buildElementType (typ: RuntimeType, spec: ElementSpec) =
        match spec.Type with
        | Definition(def) -> buildType context typ def
        | Reference(_) -> failwith "Root level element references are not allowed."
        | Name(_) -> ()

    let buildOperationService (operation: Operation) =
        let isMultipart = operation.Request.MultipartContent |> List.isEmpty |> not
        let serviceMethod = Meth.create operation.Name.LocalName |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
        let requestParameters = operation.Request.Body.Parts |> List.map (fun p -> p |> buildParameterType context isMultipart, p.Name)
        let responseParameters = operation.Response.Body.Parts |> List.map (fun p -> p |> buildParameterType context isMultipart, p.Name)
        let returnType, returnExpr = responseParameters |> List.map (fst >> fst) |> makeReturnType

        serviceMethod.ReturnType <- returnType

        let requiredHeadersExpr =
            getRequiredHeaders(operation)
            |> List.map Expr.value
            |> Arr.create<string>

        let serviceName = match operation.Version with Some v -> sprintf "%s.%s" operation.Name.LocalName v | _ -> operation.Name.LocalName

        // CodeDom doesn't support delegates, so we have to improvise
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

        serviceMethod
        |> iif (operation.Style = RpcEncoded) (fun x -> x |> Meth.addExpr ((Expr.var "writer" @-> "WriteEndElement") @% []))
        |> Meth.addExpr (Expr.code "}")
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

        let attachmentsExpr =
            if isMultipart then
                serviceMethod |> Meth.addParam<IDictionary<string,Stream>> "attachments" |> ignore
                Expr.var "attachments"
            else Expr.nil

        match operation.Documentation.TryGetValue("et") with
        | true, doc -> serviceMethod.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
        | _ -> ()

        let methodCall = ((Expr.parent @-> "MakeServiceCall") @<> [returnType]) @% [attachmentsExpr; Expr.var "writeHeader"; Expr.var "writeBody"; Expr.var "readBody"]

        if responseParameters.IsEmpty then serviceMethod |> Meth.addExpr methodCall |> ignore
        else serviceMethod |> Meth.addStmt (Stmt.ret methodCall) |> ignore 

        serviceMethod

    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Types)
    |> Seq.choose (fun x ->
        match context.GetRuntimeType(SchemaType(x.Key)) with
        | CollectionType(prtyp, _, Some(st)) -> Some(prtyp, st)
        | CollectionType(_, _, None) -> None
        | rtyp -> Some(rtyp, x.Value))
    |> Seq.iter (fun (rtyp, def) -> buildType context rtyp def)

    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Elements)
    |> Seq.choose (fun x ->
        match x.Value.Type with
        | Definition(_) -> Some(context.GetRuntimeType(SchemaElement(x.Key)), x.Value)
        | _ -> None)
    |> Seq.iter buildElementType

    let targetClass = Cls.create typeNamePath.[typeNamePath.Length - 1] |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic

    if undescribedFaults then
        targetClass.Members.Add(createXmlBookmarkReaderType()) |> ignore

    targetClass.Members.Add(portBaseTy) |> ignore
    targetClass.Members.Add(serviceTypesTy) |> ignore
    targetClass.Members.Add(createBinaryContentType()) |> ignore

    schema.Services |> List.iter (fun service ->
        let serviceTy = Cls.create service.Name |> Cls.setAttr TypeAttributes.Public |> Cls.asStatic
        service.Ports |> List.iter (fun port ->
            let portTy = CodeTypeDeclaration(port.Name, IsClass=true, TypeAttributes=TypeAttributes.Public)
            serviceTy.Members.Add(portTy) |> ignore

            portTy |> Cls.setParent (typeRefName portBaseTy.Name) |> ignore

            match port.Documentation.TryGetValue("et") with
            | true, doc -> portTy.Comments.Add(CodeCommentStatement(doc, true)) |> ignore
            | _ -> ()

            let ctor = CodeConstructor()
            ctor.Attributes <- MemberAttributes.Public
            ctor.BaseConstructorArgs.Add(CodePrimitiveExpression(port.Address)) |> ignore
            ctor.BaseConstructorArgs.Add(CodePrimitiveExpression(port.Producer)) |> ignore
            portTy.Members.Add(ctor) |> ignore

            port.Operations |> List.iter (buildOperationService >> portTy.Members.Add >> ignore))
        targetClass.Members.Add(serviceTy) |> ignore)

    context.CachedNamespaces |> Seq.iter (fun kvp -> kvp.Value |> serviceTypesTy.Members.Add |> ignore)

    let codeNamespace = CodeNamespace(String.Join(".", Array.sub typeNamePath 0 (typeNamePath.Length - 1)))
    codeNamespace.Types.Add(targetClass) |> ignore

    let assembly = Compiler.buildAssembly(codeNamespace)
    assembly.GetType(sprintf "%s.%s" codeNamespace.Name targetClass.Name)
