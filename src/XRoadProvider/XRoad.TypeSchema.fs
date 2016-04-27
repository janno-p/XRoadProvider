module internal XRoad.TypeSchema

open System
open System.Collections.Generic
open System.Globalization
open System.Xml.Linq

open XRoad.Wsdl

/// Extract numberical bound limits from current element.
let readBoundsValue name node =
    match node |> attr name with
    | Some("unbounded") -> UInt32.MaxValue
    | Some(x) -> UInt32.Parse(x)
    | _ -> 1u

/// General function for reading typed values from attributes with unqualified name.
let readValue fparse name defValue node =
    match node |> attr (xname name) with
    | Some(v) -> fparse(v)
    | _ -> defValue

/// Read minOccurs value from current element, use default value 1 if attribute is missing.
let readMinOccurs: XElement -> uint32 = readBoundsValue (xname "minOccurs")

/// Read maxOccurs value from current element, use default value 1 if attribute is missing.
let readMaxOccurs: XElement -> uint32 = readBoundsValue (xname "maxOccurs")

/// Read boolean value from attribute.
let readBoolean name node = readValue Boolean.Parse name false node

/// Read integer value from attribute.
let readInt name node = readValue Int32.Parse name 0 node

/// Read attribute contents as decimal value.
let readDecimal name node = readValue (fun x -> Decimal.Parse(x, CultureInfo.InvariantCulture)) name 0m node

/// Read boolean value which identifies if attribute is nillable.
let readNillable: XElement -> bool = readBoolean "nillable"

/// Helper method for parsing to notify about erroneous schema element definitions.
let notExpectedIn containerName (node: XElement) =
    failwithf "Element %A inside %s element was not expected at the current position!" node.Name containerName

/// Helper method for parsing to notify about schema element definition constructs which are not yet supported.
let notImplementedIn containerName (node: XElement) =
    failwithf "Element %A inside %s element is not implemented yet." node.Name containerName

/// Describes attribute usage.
type AttributeUse =
    | Optional
    | Prohibited
    | Required
    with
        static member FromNode(node) =
            match node |> attrOrDefault (xname "use") "optional" with
            | "optional" -> AttributeUse.Optional
            | "prohibited" -> AttributeUse.Prohibited
            | "required" -> AttributeUse.Required
            | x -> failwithf "Invalid attribute use value %s" x

type TypeDefinition<'T> =
    | Definition of 'T
    | Name of XName

/// Schema objects can be defined using qualified name of global definition, referencing another object with
/// `ref` attribute or give object definition in place.
type RefOrTypeDefinition<'T> =
    | Explicit of TypeDefinition<'T>
    | Reference of XName

/// Type schemas `element` node definition.
type ElementSpec =
    { Annotation: Annotation option
      Name: string option
      MinOccurs: uint32
      MaxOccurs: uint32
      IsNillable: bool
      Definition: RefOrTypeDefinition<SchemaTypeDefinition> }
    static member FromNode(node) =
        { Annotation = None
          Name = None
          MinOccurs = readMinOccurs node
          MaxOccurs = readMaxOccurs node
          IsNillable = readNillable node
          Definition = Explicit(Definition(EmptyDefinition)) }

/// Schema can give definitions simpleType or complexType; EmptyType is used when type information is not present.
and SchemaTypeDefinition =
    | EmptyDefinition
    | ComplexDefinition of ComplexTypeSpec
    | SimpleDefinition of SimpleTypeSpec

/// Wraps complex type definition.
and ComplexTypeSpec =
    { Annotation: Annotation option
      IsAbstract: bool
      Content: ComplexTypeContent }

/// Simple types can restrict existing simple types or combine existing simple types to list and unions.
and SimpleTypeSpec =
    | Restriction of SimpleTypeRestrictionSpec * Annotation option
    | ListDef
    | Union of UnionSpec

/// Wraps `complexType` node content definition.
and ComplexTypeContent =
    | Empty
    | SimpleContent of SimpleContentSpec
    | ComplexContent of ComplexContentSpec
    | Particle of ComplexTypeContentSpec

/// Describes other simpleType definition to restrict and wraps the definition of restrictions.
and SimpleTypeRestrictionSpec =
    { Base: XName
      SimpleType: SimpleTypeSpec option
      Content: RestrictionContent list }

/// Wraps `union` node definition (types included in union).
and UnionSpec =
    { MemberTypeNames: XName list
      MemberTypes: SimpleTypeSpec list }

/// Complex type `simpleContent` either restricts or extends existing simple types.
and SimpleContentSpec =
    | Restriction of SimpleContentRestrictionSpec
    | Extension of ExtensionSpec

/// Complex type `complexContent` either restricts or extends existing types.
and ComplexContentSpec =
        | Restriction of ComplexContentRestrictionSpec
        | Extension of ExtensionSpec

/// Complex type content defines elements and attributes that are allowed in that type.
and ComplexTypeContentSpec =
    { Content: ComplexTypeParticle option
      Attributes: AttributeSpec list
      AttributeGroups: AttributeGroupSpec list }

/// Various options to restrict simple type definitions.
and RestrictionContent =
    | MinExclusive of decimal
    | MinInclusive of decimal
    | MaxExclusive of decimal
    | MaxInclusive of decimal
    | TotalDigits
    | FractionDigits
    | Length
    | MinLength of int
    | MaxLength
    | Enumeration of string
    | WhiteSpace
    | Pattern of string

/// Simple content restriction defines simple type to restrict and restrictions to apply on that type.
and SimpleContentRestrictionSpec =
    { Base: XName
      SimpleType: SimpleTypeSpec option
      Content: RestrictionContent list
      Attributes: AttributeSpec list }

/// Extension identifies type to extend and gives definition for extension.
and ExtensionSpec =
    { Base: XName
      Content: ComplexTypeContentSpec }

/// Complex content restriction identifies type to restrict and gives definition for restrictions.
and ComplexContentRestrictionSpec =
    { Base: XName
      Content: ComplexTypeContentSpec }

/// Complex type can define its content by referencing global group definitions, list of possible elements,
/// alternative combinations of elements, or element sequences.
and ComplexTypeParticle =
    | Group
    | All of AllSpec
    | Choice of ParticleSpec
    | Sequence of ParticleSpec

/// Single attribute definition.
and AttributeSpec =
    { Annotation: Annotation option
      Name: string option
      RefOrType: RefOrTypeDefinition<SimpleTypeSpec>
      Use: AttributeUse
      /// Used for SOAP-encoded array-s.
      ArrayType: (XName * int) option }

/// Elements defined in `all` node can appear in any order.
and AllSpec =
    { MinOccurs: uint32
      MaxOccurs: uint32
      Elements: ElementSpec list }

/// Defines alternatives for current choice or sequence.
and ParticleSpec =
    { Annotation: string option
      MaxOccurs: uint32
      MinOccurs: uint32
      Content: ParticleContent list }

/// Single choice alternative or sequence can contain `any` node to mark acceptance of any element; sub-choice nodes;
/// concrete element definitions; references to predefined element groups; or element sequences.
and ParticleContent =
    | Any
    | Choice of ParticleSpec
    | Element of ElementSpec
    | Group
    | Sequence of ParticleSpec

/// Wrap multiple attribute definitions into predefined group.
and AttributeGroupSpec =
    { Annotation: string
      Attributes: AttributeSpec list
      AttributeGroups: AttributeGroupSpec list
      AllowAny: bool }

/// Documentation info extracted from service descriptions.
and Annotation = { AppInfo: XElement list }

/// Root type to hold definition for entire type schema.
type SchemaNode =
    { QualifiedAttributes: bool
      QualifiedElements: bool
      TargetNamespace: XNamespace
      Attributes: IDictionary<XName,AttributeSpec>
      Elements: IDictionary<XName,ElementSpec>
      Types: IDictionary<XName,SchemaTypeDefinition>
      AttributeGroups: IDictionary<XName,TypeDefinition<AttributeGroupSpec>> }
    /// Merge schema node with another defining same namespace.
    member this.Merge(other: SchemaNode) =
        if this.QualifiedAttributes <> other.QualifiedAttributes then
            failwith "Same namespace has inconsistent values for qualified attributes in different schema files."
        if this.QualifiedElements <> other.QualifiedElements then
            failwith "Same namespace has inconsistent values for qualified elements in different schema files."
        other.Attributes |> Seq.iter this.Attributes.Add
        other.Elements |> Seq.iter this.Elements.Add
        other.Types |> Seq.iter this.Types.Add
        other.AttributeGroups |> Seq.iter this.AttributeGroups.Add
    /// Initializes empty SchemaNode from given `schema` node.
    static member FromNode(node) =
        { QualifiedAttributes = node |> isQualified (xname "attributeFormDefault")
          QualifiedElements = node |> isQualified (xname "elementFormDefault")
          TargetNamespace = node |> attrOrDefault (xname "targetNamespace") "" |> xns
          Attributes = Dictionary<_,_>()
          Elements = Dictionary<_,_>()
          Types = Dictionary<_,_>()
          AttributeGroups = Dictionary<_,_>() }

module Parser =
    /// Keeps internal state of parsing for current node.
    type private State =
        | Begin
        | Header
        | Annotation
        | Content
        | Particle
        | Attribute
        | AnyAttribute
        | TypeSpec
        | Other

    /// Extracts documentation from annotation element definition.
    let private parseAnnotation (parentNode: XElement): Annotation option =
        match parentNode.Element(xnsname "annotation" XmlNamespace.Xsd) with
        | null -> None
        | node ->
            match node.Elements(xnsname "appinfo" XmlNamespace.Xsd) |> List.ofSeq with
            | [] -> None
            | elements -> Some({ AppInfo = elements })

    /// Extracts complexType specification from schema definition.
    let rec private parseComplexType (node: XElement): ComplexTypeSpec =
        let parseChildElements() =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "simpleContent", (Begin | Annotation) ->
                    Content, Some(ComplexTypeContent.SimpleContent(parseSimpleContent node))
                | Xsd "complexContent", (Begin | Annotation) ->
                    Content, Some(ComplexTypeContent.ComplexContent(parseComplexContent node))
                | Xsd "choice", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.Choice(parseChoice(node))); Attributes = []; AttributeGroups = [] }))
                | Xsd "group", (Begin | Annotation) ->
                    Particle, node |> notImplementedIn "complexType"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.Sequence(parseSequence node)); Attributes = []; AttributeGroups = [] }))
                | Xsd "all", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.All(parseAll node)); Attributes = []; AttributeGroups = [] }))
                | Xsd "attribute", (Begin | Annotation | Particle | Attribute) ->
                    let attribute = parseAttribute node
                    let content = match spec with
                                  | Some(ComplexTypeContent.Particle(content)) -> { content with Attributes = content.Attributes @ [attribute] }
                                  | None -> { Content = None; Attributes = [attribute]; AttributeGroups = [] }
                                  | _ -> node |> notExpectedIn "complexType"
                    Attribute, Some(ComplexTypeContent.Particle(content))
                | Xsd "attributeGroup", (Begin | Annotation | Particle | Attribute) ->
                    let attributeGroup = parseAttributeGroup node
                    let content = match spec with
                                  | Some(ComplexTypeContent.Particle(content)) -> { content with AttributeGroups = content.AttributeGroups @ [attributeGroup] }
                                  | None -> { Content = None; Attributes = []; AttributeGroups = [attributeGroup] }
                                  | _ -> node |> notExpectedIn "complexType"
                    Attribute, Some(ComplexTypeContent.Particle(content))
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, node |> notImplementedIn "complexType"
                | _ ->
                    node |> notExpectedIn "complexType"
                ) (Begin, None)
                |> snd
                |> Option.orDefault ComplexTypeContent.Empty
        { IsAbstract = node |> readBoolean "abstract"; Content = parseChildElements(); Annotation = parseAnnotation(node) }

    /// Extracts complexType-s simpleContent element specification from schema definition.
    and private parseSimpleContent (node: XElement): SimpleContentSpec =
        let content =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "restriction", (Begin | Annotation) ->
                    Content, Some(SimpleContentSpec.Restriction(parseSimpleContentRestriction node))
                | Xsd "extension", (Begin | Annotation) ->
                    Content, Some(SimpleContentSpec.Extension(parseExtension node))
                | _ -> node |> notExpectedIn "simpleContent"
                ) (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element simpleContent is expected to contain either restriction or extension element."

    /// Extracts complexType-s complexContent element specification from schema definition.
    and private parseComplexContent (node: XElement): ComplexContentSpec =
        let content =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexContentSpec option) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "restriction", (Begin | Annotation) ->
                    Content, Some(ComplexContentSpec.Restriction(parseComplexContentRestriction node))
                | Xsd "extension", (Begin | Annotation) ->
                    Content, Some(ComplexContentSpec.Extension(parseExtension node))
                | _ -> node |> notExpectedIn "complexContent")
                (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element complexContent is expected to contain either restriction or extension element."

    /// Extracts choice or sequence element specification from schema definition.
    and private parseParticle particleName (node: XElement): ParticleSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: ParticleSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, { spec with Annotation = Some(node.Value) }
            | Xsd "any", _ ->
                Content, { spec with Content = spec.Content @ [ParticleContent.Any] }
            | Xsd "choice", _ ->
                Content, { spec with Content = spec.Content @ [ParticleContent.Choice(parseChoice(node))] }
            | Xsd "element", _ ->
                Content, { spec with Content = spec.Content @ [ParticleContent.Element(parseElement(node))] }
            | Xsd "group", _ ->
                Content, node |> notImplementedIn particleName
            | Xsd "sequence", _ ->
                Content, { spec with Content = spec.Content @ [ParticleContent.Sequence(parseSequence(node))] }
            | _ -> node |> notExpectedIn particleName
            ) (Begin, { Annotation = None
                        MaxOccurs = readMaxOccurs node
                        MinOccurs = readMinOccurs node
                        Content = [] })
        |> snd

    and private parseChoice = parseParticle "choice"
    and private parseSequence = parseParticle "sequence"

    /// Extracts `all` element specification from schema definition.
    and private parseAll (node: XElement): AllSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: AllSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, spec
            | Xsd "element", (Begin | Annotation | Content) ->
                Content, { spec with Elements = spec.Elements @ [parseElement(node)] }
            | _ -> node |> notExpectedIn "all"
            ) (Begin, { MinOccurs = readMinOccurs node
                        MaxOccurs = readMaxOccurs node
                        Elements = [] })
        |> snd

    /// Extracts `attribute` element specification from schema definition.
    and private parseAttribute (node: XElement): AttributeSpec =
        // Handle SOAP-encoded array definition to get array dimensions.
        let arrayType =
            match node |> attr (xnsname "arrayType" XmlNamespace.Wsdl) with
            | Some value ->
                let ns, name = match value.Split(':') with
                                   | [| local |] -> node.GetDefaultNamespace().NamespaceName, local
                                   | [| prefix; local |] -> node.GetNamespaceOfPrefix(prefix).NamespaceName, local
                                   | _ -> failwithf "Invalid array type: %A" value
                match System.Text.RegularExpressions.Regex.Match(name, @"^(\w+)(\[\])+$") with
                | m when m.Success -> Some(xnsname m.Groups.[1].Value ns, m.Groups.[2].Captures.Count)
                | _ -> failwithf "Invalid array type: %A" value
            | _ -> None
        let attrUse = AttributeUse.FromNode(node)
        // Parse attribute type information.
        match node |> attr (xname "ref") with
        | Some refv ->
            match node |> attr (xname "name") with
            | Some _ -> failwith "Attribute element name and ref attribute cannot be present at the same time."
            | _ -> { Name = None; RefOrType = Reference(parseXName node refv); ArrayType = arrayType; Use = attrUse; Annotation = parseAnnotation(node) }
        | _ ->
            let name = node |> reqAttr (xname "name")
            match node |> attr (xname "type") with
            | Some value -> { Name = Some(name); RefOrType = Explicit(Name(parseXName node value)); ArrayType = arrayType; Use = attrUse; Annotation = parseAnnotation(node) }
            | _ ->
                node.Elements()
                |> Seq.fold (fun (state, spec) node ->
                    match node, state with
                    | Xsd "annotation", Begin -> Annotation, spec
                    | Xsd "simpleType", (Begin | Annotation) -> TypeSpec, Some(parseSimpleType(node))
                    | _ -> node |> notExpectedIn "attribute"
                    ) (Begin, None)
                |> (fun (_, typ) -> match typ with
                                    | Some(typ) -> { Name = Some(name); RefOrType = Explicit(Definition(typ)); ArrayType = arrayType; Use = attrUse; Annotation = parseAnnotation(node) }
                                    | _ -> failwithf "Attribute element %s type definition is missing." name)

    /// Extracts complexType-s `simpleContent` element specification from schema definition.
    and private parseSimpleContentRestriction (node: XElement): SimpleContentRestrictionSpec =
        node |> notImplementedIn "simpleContent restriction"

    /// Extracts `extension` element specification from schema definition.
    and private parseExtension (node: XElement): ExtensionSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexTypeContentSpec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | (Xsd "group" | Xsd "all" | Xsd "choice"), (Begin | Annotation) ->
                    node |> notImplementedIn "extension"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle, { spec with Content = Some(ComplexTypeParticle.Sequence(parseSequence node)) }
                | (Xsd "attribute" | Xsd "attributeGroup"), (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with Attributes = spec.Attributes @ [parseAttribute node] }
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    node |> notImplementedIn "extension"
                | _ -> node |> notExpectedIn "extension"
                ) (Begin, { Content = None; Attributes = []; AttributeGroups = [] })
            |> snd
        { Base = node |> reqAttr (xname "base") |> parseXName node
          Content = parseChildElements() }

    /// Extracts complexType-s complexContent-s `restriction` element specification from schema definition.
    and private parseComplexContentRestriction (node: XElement): ComplexContentRestrictionSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexTypeContentSpec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "group", (Begin | Annotation)
                | Xsd "all", (Begin | Annotation)
                | Xsd "choice", (Begin | Annotation) ->
                    node |> notImplementedIn "complexContent restriction"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle,  { spec with Content = Some(ComplexTypeParticle.Sequence(parseSequence node)) }
                | Xsd "attribute", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with Attributes = spec.Attributes @ [parseAttribute node] }
                | Xsd "attributeGroup", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with AttributeGroups = spec.AttributeGroups @ [parseAttributeGroup node] }
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    node |> notImplementedIn "complexContent restriction"
                | _ ->
                    node |> notExpectedIn "complexContent restriction"
                ) (Begin, { Content = None; Attributes = []; AttributeGroups = [] })
            |> snd
        { Base = node |> reqAttr (xname "base") |> parseXName node
          Content = parseChildElements() }

    /// Extracts `element` element specification from schema definition.
    and private parseElement (node: XElement): ElementSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: TypeDefinition<_> option) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "simpleType", (Begin | Annotation) ->
                    TypeSpec, Some(Definition(SimpleDefinition(parseSimpleType node)))
                | Xsd "complexType", (Begin | Annotation) ->
                    TypeSpec, Some(Definition(ComplexDefinition(parseComplexType node)))
                | (Xsd "unique" | Xsd "key" | Xsd "keyref"), (Begin | Annotation | TypeSpec | Other) ->
                    node |> notImplementedIn "element"
                | _ -> node |> notExpectedIn "element"
                ) (Begin, None)
            |> (fun (_, spec) -> spec |> Option.orDefault(Definition(EmptyDefinition)))
        let elementSpec = ElementSpec.FromNode(node)
        match node |> attr (xname "ref") with
        | Some refv ->
            match node |> attr (xname "name") with
            | Some _ -> failwith "Attribute element name and ref attribute cannot be present at the same time."
            | _ -> { elementSpec with Definition = Reference(parseXName node refv) }
        | _ ->
            { elementSpec with
                Annotation = parseAnnotation(node)
                Name = Some(node |> reqAttr (xname "name"))
                Definition = match node |> attr (xname "type") with
                             | Some value -> Explicit(Name(parseXName node value))
                             | _ -> Explicit(parseChildElements()) }

    /// Extracts `simpleType` element specification from schema definition.
    and private parseSimpleType (node: XElement): SimpleTypeSpec =
        let annotation = parseAnnotation(node)
        let content =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "restriction", (Begin | Annotation) ->
                    Content, Some(SimpleTypeSpec.Restriction(parseSimpleTypeRestriction node, annotation))
                | Xsd "union", (Begin | Annotation) ->
                    Content, Some(SimpleTypeSpec.Union(parseUnion node))
                | Xsd "list", (Begin | Annotation) ->
                    Content, node |> notImplementedIn "simpleType"
                | _ -> node |> notExpectedIn "simpleType"
                ) (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element simpleType is expected to contain either restriction, list or union element."

    /// Extracts simpleType-s `restriction` element specification from schema definition.
    and private parseSimpleTypeRestriction (node: XElement): SimpleTypeRestrictionSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: SimpleTypeRestrictionSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                state, spec
            | Xsd "simpleType", (Begin | Annotation) ->
                TypeSpec, node |> notImplementedIn "simpleType restriction"
            | Xsd "enumeration", (Begin | Annotation | TypeSpec | Content) ->
                let value = node |> reqAttr(xname "value")
                Content, { spec with Content = spec.Content @ [Enumeration(value)] }
            | Xsd "minLength", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MinLength(node |> readInt "value")] }
            | Xsd "minInclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MinInclusive(node |> readDecimal "value") ] }
            | Xsd "maxInclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MaxInclusive(node |> readDecimal "value") ] }
            | Xsd "minExclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MinExclusive(node |> readDecimal "value") ] }
            | Xsd "maxExclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MaxExclusive(node |> readDecimal "value") ] }
            | Xsd "pattern", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [Pattern(node |> reqAttr(xname "value"))] }
            | (Xsd "totalDigits" | Xsd "fractionDigits" | Xsd "length" | Xsd "minLength" | Xsd "maxLength" | Xsd "whiteSpace"), (Begin | Annotation | TypeSpec | Content) ->
                Content, node |> notImplementedIn "simpleType restriction"
            | (Xsd "attribute" | Xsd "attributeGroup"), (Begin | Annotation | TypeSpec | Content | Attribute) ->
                Attribute, node |> notImplementedIn "simpleType restriction"
            | Xsd "anyAttribute", (Begin | Annotation | TypeSpec | Content | Attribute) ->
                Other, node |> notImplementedIn "simpleType restriction"
            | _ -> node |> notExpectedIn "simpleType restriction"
            ) (Begin, { Base = node |> reqAttr (xname "base") |> parseXName node
                        SimpleType = None
                        Content = [] })
        |> snd

    /// Extracts simpleType-s `union` element specification from schema definition.
    and private parseUnion (node: XElement): UnionSpec =
        { MemberTypeNames =
            match node |> attr(xname "memberTypes") with
            | Some(str) ->
                str.Split(' ')
                |> Array.map (fun x ->
                    match x.Split(':') with
                    | [| name |] -> xname name
                    | [| ns; name |] -> xnsname name <| node.GetNamespaceOfPrefix(ns).NamespaceName
                    | _ -> failwithf "Invalid member type name %s" x)
                |> List.ofArray
            | None -> []
          MemberTypes =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "simpleType", (Begin | Annotation) ->
                    Content, spec @ [parseSimpleType(node)]
                | _ -> node |> notExpectedIn "union"
                ) (Begin, [])
            |> snd }

    /// Extracts `attributeGroup` element specification from schema definition.
    and private parseAttributeGroup (node: XElement): AttributeGroupSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: AttributeGroupSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, spec
            | Xsd "attribute", (Begin | Annotation | Attribute) ->
                Attribute, { spec with Attributes = (parseAttribute node)::spec.Attributes }
            | Xsd "attributeGroup", (Begin | Annotation | Attribute) ->
                Attribute, { spec with AttributeGroups = (parseAttributeGroup node)::spec.AttributeGroups }
            | Xsd "anyAttribute", (Begin | Annotation | Attribute | Other) ->
                Other, node |> notImplementedIn "attributeGroup"
            | _ -> node |> notExpectedIn "attributeGroup"
            ) (Begin, { Annotation = ""; Attributes = []; AttributeGroups = []; AllowAny = false })
        |> snd

    /// Parses `schema` node contents and completes schemaNode definition details.
    let internal parseSchemaNode schemaNode (node: XElement) =
        node.Elements()
        |> Seq.fold (fun (state, snode, includes, imports) node ->
            match node, state with
            | Xsd "annotation", _ ->
                state, snode, includes, imports
            | Xsd "include", (Begin | Header) ->
                let schloc = node |> reqAttr (xname "schemaLocation")
                Header, snode, (schloc :: includes), imports
            | Xsd "import", (Begin | Header) ->
                let ns = node |> attrOrDefault (xname "namespace") ""
                let schloc = node |> attr (xname "schemaLocation")
                Header, snode, includes, ((xns ns, schloc) :: imports)
            | Xsd "redefine", (Begin | Header) ->
                node |> notImplementedIn "schema"
            | Xsd "complexType", _ ->
                let name = node |> reqAttr (xname "name")
                let typ = ComplexDefinition(parseComplexType node)
                snode.Types.Add(xnsname name snode.TargetNamespace.NamespaceName, typ)
                TypeSpec, snode, includes, imports
            | Xsd "element", _ ->
                let element = parseElement node
                match element.Name with
                | None -> failwith "`name` attribute is required if the parent element is the schema element"
                | Some(name) -> snode.Elements.Add(xnsname name snode.TargetNamespace.NamespaceName, element)
                TypeSpec, snode, includes, imports
            | Xsd "simpleType", _ ->
                let name = node |> reqAttr (xname "name")
                let typ = SimpleDefinition(parseSimpleType node)
                snode.Types.Add(xnsname name snode.TargetNamespace.NamespaceName, typ)
                TypeSpec, snode, includes, imports
            | Xsd "attribute", _ ->
                let attribute = parseAttribute(node)
                snode.Attributes.Add(xnsname attribute.Name.Value snode.TargetNamespace.NamespaceName, attribute)
                TypeSpec, snode, includes, imports
            | Xsd "attributeGroup", _ ->
                let ag = node |> parseAttributeGroup
                match node |> attr (xname "name"), node |> attr (xname "ref") with
                | Some(_), Some(_) ->
                    failwithf "Name and ref attributes cannot both be present (%A)" node.Name.LocalName
                | Some(name), None ->
                    snode.AttributeGroups.Add(xnsname name snode.TargetNamespace.NamespaceName, Definition(ag))
                | None, Some(ref) ->
                    let name =
                        match ref.Split(':') with
                        | [| nm |] -> xnsname nm snode.TargetNamespace.NamespaceName
                        | [| pr; nm |] -> xnsname nm (node.GetNamespaceOfPrefix(pr).NamespaceName)
                        | _ -> failwith "wrong ref"
                    snode.AttributeGroups.Add(name, Definition(ag))
                | _ ->
                    node |> notImplementedIn "schema"
                TypeSpec, snode, includes, imports
            | (Xsd "group" | Xsd "notation"), _ -> node |> notImplementedIn "schema"
            | _ -> node |> notExpectedIn "schema"
            ) (Begin, schemaNode, [], [])
        |> (fun (_,a,b,c) -> (a,b,c))

    let fixUri (contextUri: Uri option) path =
        match Uri.TryCreate(path, UriKind.Absolute), contextUri with
        | (true, absUri), _ -> absUri
        | _, Some(uri) -> match Uri.TryCreate(uri, path) with
                          | true, absUri -> absUri
                          | _ -> failwithf "Unable to detect uri `%s` in the context of `%A`." path uri
        | _ -> failwithf "Could not resolve uri `%s`." path

    /// Parses all definitions in given schema node.
    let rec private findSchemaNode (schemaUri: Uri) (schemaLookup: Dictionary<(string * string),SchemaNode>) (documentSchemas: Map<_,_>) node =
        let schemaNode = SchemaNode.FromNode(node)
        // Use previously parsed schema if present.
        match schemaLookup.TryGetValue((schemaNode.TargetNamespace.NamespaceName, schemaUri.ToString())) with
        | false, _ ->
            let schema, includes, imports = node |> parseSchemaNode schemaNode
            schemaLookup.Add((schemaNode.TargetNamespace.NamespaceName, schemaUri.ToString()), schema)
            // Parse imported schemas also.
            imports
            |> List.filter (fun (ns, _) -> XmlNamespace.predefined |> List.exists ((=) ns.NamespaceName) |> not)
            |> List.iter (fun (ns, uri) ->
                match uri, documentSchemas.TryFind(ns.NamespaceName) with
                | None, Some(_) -> ()
                | _ ->
                    let path = (uri |> Option.orDefault(ns.NamespaceName)) |> fixUri (Some schemaUri)
                    let schemaNode =
                        XDocument.Load(path.ToString()).Element(xnsname "schema" XmlNamespace.Xsd)
                        |> findSchemaNode path schemaLookup documentSchemas
                    if schemaNode.TargetNamespace <> ns then
                        failwith "Imported type schema should define same target namespace as the schema importing it.")
            // Parse included schemas into target namespace.
            includes
            |> List.iter (fun uri ->
                let path = uri |> fixUri (Some schemaUri)
                let schemaNode = XDocument.Load(path.ToString()).Element(xnsname "schema" XmlNamespace.Xsd)
                                 |> findSchemaNode path schemaLookup documentSchemas
                if schemaNode.TargetNamespace <> schema.TargetNamespace then
                    failwith "Included type schema should define same target namespace as the schema including it.")
            schema
        | true, schema -> schema

    let private getDocumentSchemas (typesNode: XElement) =
        typesNode.Elements(xnsname "schema" XmlNamespace.Xsd)
        |> Seq.choose (fun schemaNode ->
            match schemaNode.Attribute(xname "targetNamespace") with
            | null -> None
            | attr -> Some(attr.Value, schemaNode))
        |> Map.ofSeq

    /// Parses all type schemas defined and referenced in current WSDL document.
    let parseSchema path (definitions: XElement) =
        match definitions.Element(xnsname "types" XmlNamespace.Wsdl) with
        | null -> Map.empty
        | typesNode ->
            let schemaLookup = Dictionary<_,_>()
            let uri = fixUri None path
            let documentSchemas = getDocumentSchemas typesNode
            documentSchemas |> Map.iter (fun _ node -> findSchemaNode uri schemaLookup documentSchemas node |> ignore)
            schemaLookup
            |> Seq.fold (fun (mergedSchemas: Dictionary<string,SchemaNode>) kvp ->
                match mergedSchemas.TryGetValue (fst kvp.Key) with
                | true, existingSchema -> existingSchema.Merge(kvp.Value)
                | _ -> mergedSchemas.Add(fst kvp.Key, kvp.Value)
                mergedSchemas) (Dictionary<_,_>())
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
