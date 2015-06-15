module internal XRoad.TypeSchema

open System
open System.Collections.Generic
open System.Xml.Linq

open XRoad.Common

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

/// Schema objects can be defined using qualified name of global definition, referencing another object with
/// `ref` attribute or give object definition in place.
type SchemaObject<'T> =
    | Name of XName
    | Reference of XName
    | Definition of 'T
    with
        static member FromNode(node) =
            match node |> attr (xname "name"), node |> attr (xname "ref") with
            | Some(_), Some(_) ->
                failwithf "Name and ref attributes cannot both be present (%A)" node.Name.LocalName
            | Some(name), _ ->
                Some(Name(xname name))
            | _, Some(ref) ->
                match ref.Split(':') with
                | [| nm |] -> Some(Reference(xname nm))
                | [| pr; nm |] -> Some(Reference(xnsname nm <| node.GetNamespaceOfPrefix(pr).NamespaceName))
                | _ -> failwith "wrong ref"
            | _ -> None

/// Type schemas `element` node definition.
type ElementSpec =
    { Name: string option
      MinOccurs: uint32
      MaxOccurs: uint32
      IsNillable: bool
      Type: SchemaObject<SchemaType> }
    static member FromNode(node) =
        { Name = None
          MinOccurs = readMinOccurs node
          MaxOccurs = readMaxOccurs node
          IsNillable = readNillable node
          Type = Definition(EmptyType) }

/// Schema can give definitions simpleType or complexType; EmptyType is used when type information is not present.
and SchemaType =
    | EmptyType
    | ComplexType of ComplexTypeSpec
    | SimpleType of SimpleTypeSpec

/// Wraps complex type definition.
and ComplexTypeSpec =
    { IsAbstract: bool
      Content: ComplexTypeContent }

/// Simple types can restrict existing simple types or combine existing simple types to list and unions.
and SimpleTypeSpec =
    | Restriction of SimpleTypeRestrictionSpec
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
      Attributes: AttributeSpec list }

/// Various options to restrict simple type definitions.
and RestrictionContent =
    | MinExclusive
    | MinInclusive
    | MaxExclusive
    | MaxInclusive
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
    | Choice of ChoiceSpec
    | Sequence of SequenceSpec

/// Single attribute definition.
and AttributeSpec =
    { Name: string option
      RefOrType: SchemaObject<SimpleTypeSpec>
      Use: AttributeUse
      /// Used for SOAP-encoded array-s.
      ArrayType: (XName * int) option }

/// Elements defined in `all` node can appear in any order.
and AllSpec =
    { MinOccurs: uint32
      MaxOccurs: uint32
      Elements: ElementSpec list }

/// Defines alternatives for current choice.
and ChoiceSpec =
    { Annotation: string option
      MaxOccurs: uint32
      MinOccurs: uint32
      Content: ChoiceContent list }

/// Sequence can contain `any` element to mark acceptance of any element; concrete element definitions;
/// references to predefined element groups; alternatives via choice elements; or subsequences.
and SequenceContent =
    | Any
    | Element of ElementSpec
    | Group
    | Choice of ChoiceSpec
    | Sequence of SequenceSpec

/// Wraps single `sequence` node definition.
and SequenceSpec =
    { MinOccurs: uint32
      MaxOccurs: uint32
      Content: SequenceContent list }

/// Single choice alternative can contain `any` node to mark acceptance of any element; sub-choice nodes;
/// concrete element definitions; references to predefined element groups; or element sequences.
and ChoiceContent =
    | Any
    | Choice of ChoiceSpec
    | Element of ElementSpec
    | Group
    | Sequence of SequenceSpec

/// Wrap multiple attribute definitions into predefined group.
type AttributeGroupSpec =
    { Annotation: string
      Attributes: AttributeSpec list
      AllowAny: bool }

/// Root type to hold definition for entire type schema.
type SchemaNode =
    { QualifiedAttributes: bool
      QualifiedElements: bool
      TargetNamespace: XNamespace
      Includes: Uri list
      Imports: (XNamespace * string option) list
      Attributes: IDictionary<XName,AttributeSpec>
      Elements: IDictionary<XName,ElementSpec>
      Types: IDictionary<XName,SchemaType>
      AttributeGroups: IDictionary<XName,SchemaObject<AttributeGroupSpec>> }
    /// Initializes empty SchemaNode from given `schema` node.
    static member FromNode(node) =
        { QualifiedAttributes = node |> isQualified (xname "attributeFormDefault")
          QualifiedElements = node |> isQualified (xname "elementFormDefault")
          TargetNamespace = node |> attrOrDefault (xname "targetNamespace") "" |> xns
          Includes = []
          Imports = []
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

    /// Extracts complexType specification from schema definition.
    let rec private parseComplexType (node: XElement): ComplexTypeSpec =
        let parseChildElements () =
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
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.Choice(parseChoice(node))); Attributes = [] }))
                | Xsd "group", (Begin | Annotation) ->
                    Particle, node |> notImplementedIn "complexType"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.Sequence(parseSequence node)); Attributes = [] }))
                | Xsd "all", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.All(parseAll node)); Attributes = [] }))
                | Xsd "attribute", (Begin | Annotation | Particle | Attribute) ->
                    let attribute = parseAttribute node
                    let content = match spec with
                                  | Some(ComplexTypeContent.Particle(content)) -> { content with Attributes = content.Attributes @ [attribute] }
                                  | None -> { Content = None; Attributes = [attribute] }
                                  | _ -> node |> notExpectedIn "complexType"
                    Attribute, Some(ComplexTypeContent.Particle(content))
                | Xsd "attributeGroup", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, node |> notImplementedIn "complexType"
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, node |> notImplementedIn "complexType"
                | _ ->
                    node |> notExpectedIn "complexType"
                ) (Begin, None)
            |> snd
            |> Option.orDefault ComplexTypeContent.Empty
        { IsAbstract = node |> readBoolean "abstract"; Content = parseChildElements() }

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

    /// Extracts choice element specification from schema definition.
    and private parseChoice (node: XElement): ChoiceSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: ChoiceSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, { spec with Annotation = Some(node.Value) }
            | Xsd "any", _ ->
                Content, { spec with Content = [ChoiceContent.Any] @ spec.Content }
            | Xsd "choice", _ ->
                Content, { spec with Content = [ChoiceContent.Choice(parseChoice(node))] @ spec.Content }
            | Xsd "element", _ ->
                Content, { spec with Content = [ChoiceContent.Element(parseElement(node))] @ spec.Content }
            | Xsd "group", _ ->
                Content, node |> notImplementedIn "choice"
            | Xsd "sequence", _ ->
                Content, { spec with Content = [ChoiceContent.Sequence(parseSequence(node))] @ spec.Content }
            | _ -> node |> notExpectedIn "choice"
            ) (Begin, { Annotation = None
                        MaxOccurs = readMaxOccurs node
                        MinOccurs = readMinOccurs node
                        Content = [] })
        |> snd

    /// Extracts sequence element specification from schema definition.
    and private parseSequence (node: XElement): SequenceSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: SequenceSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, spec
            | Xsd "any", (Begin | Annotation | Content) ->
                Content, { spec with Content = [SequenceContent.Any] @ spec.Content }
            | Xsd "choice", (Begin | Annotation | Content) ->
                Content, { spec with Content = [SequenceContent.Choice(parseChoice(node))] @ spec.Content }
            | Xsd "group", (Begin | Annotation | Content) ->
                Content, node |> notImplementedIn "sequence"
            | Xsd "sequence", (Begin | Annotation | Content) ->
                Content, { spec with Content = [SequenceContent.Sequence(parseSequence(node))] @ spec.Content }
            | Xsd "element", (Begin | Annotation | Content) ->
                Content, { spec with Content = [SequenceContent.Element(parseElement(node))] @ spec.Content }
            | _ -> node |> notExpectedIn "sequence"
            ) (Begin, { MinOccurs = readMinOccurs node
                        MaxOccurs = readMaxOccurs node
                        Content = [] })
        |> snd

    /// Extracts `all` element specification from schema definition.
    and private parseAll (node: XElement): AllSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: AllSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, spec
            | Xsd "element", (Begin | Annotation | Content) ->
                Content, { spec with Elements = [parseElement(node)] @ spec.Elements }
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
            | _ -> { Name = None; RefOrType = Reference(parseXName node refv); ArrayType = arrayType; Use = attrUse }
        | _ ->
            let name = node |> reqAttr (xname "name")
            match node |> attr (xname "type") with
            | Some value -> { Name = Some(name); RefOrType = Name(parseXName node value); ArrayType = arrayType; Use = attrUse }
            | _ ->
                node.Elements()
                |> Seq.fold (fun (state, spec) node ->
                    match node, state with
                    | Xsd "annotation", Begin -> Annotation, spec
                    | Xsd "simpleType", (Begin | Annotation) -> TypeSpec, Some(parseSimpleType(node))
                    | _ -> node |> notExpectedIn "attribute"
                    ) (Begin, None)
                |> (fun (_, typ) -> match typ with
                                    | Some(typ) -> { Name = Some(name); RefOrType = Definition(typ); ArrayType = arrayType; Use = attrUse }
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
                ) (Begin, { Content = None; Attributes = [] })
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
                    node |> notImplementedIn "complexContent restriction"
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    node |> notImplementedIn "complexContent restriction"
                | _ ->
                    node |> notExpectedIn "complexContent restriction"
                ) (Begin, { Content = None; Attributes = [] })
            |> snd
        { Base = node |> reqAttr (xname "base") |> parseXName node
          Content = parseChildElements() }

    /// Extracts `element` element specification from schema definition.
    and private parseElement (node: XElement): ElementSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: SchemaObject<_> option) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "simpleType", (Begin | Annotation) ->
                    TypeSpec, Some(Definition(SimpleType(parseSimpleType node)))
                | Xsd "complexType", (Begin | Annotation) ->
                    TypeSpec, Some(Definition(ComplexType(parseComplexType node)))
                | (Xsd "unique" | Xsd "key" | Xsd "keyref"), (Begin | Annotation | TypeSpec | Other) ->
                    node |> notImplementedIn "element"
                | _ -> node |> notExpectedIn "element"
                ) (Begin, None)
            |> (fun (_, spec) -> spec |> Option.orDefault(Definition(EmptyType)))
        let elementSpec = ElementSpec.FromNode(node)
        match node |> attr (xname "ref") with
        | Some refv ->
            match node |> attr (xname "name") with
            | Some _ -> failwith "Attribute element name and ref attribute cannot be present at the same time."
            | _ -> { elementSpec with Type = Reference(parseXName node refv) }
        | _ ->
            { elementSpec with
                Name = Some(node |> reqAttr (xname "name"))
                Type = match node |> attr (xname "type") with
                       | Some value -> Name(parseXName node value)
                       | _ -> parseChildElements() }

    /// Extracts `simpleType` element specification from schema definition.
    and private parseSimpleType (node: XElement): SimpleTypeSpec =
        let content =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "restriction", (Begin | Annotation) ->
                    Content, Some(SimpleTypeSpec.Restriction(parseSimpleTypeRestriction node))
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
                Content, { spec with Content = [Enumeration(value)] @ spec.Content }
            | Xsd "minLength", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = [MinLength(node |> readInt "value")] @ spec.Content }
            | Xsd "pattern", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = [Pattern(node |> reqAttr(xname "value"))] @ spec.Content }
            | (Xsd "minExclusive" | Xsd "minInclusive" | Xsd "maxExclusive" | Xsd "maxInclusive" | Xsd "totalDigits" | Xsd "fractionDigits" | Xsd "length" | Xsd "minLength" | Xsd "maxLength" | Xsd "whiteSpace"), (Begin | Annotation | TypeSpec | Content) ->
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
                    Content, [parseSimpleType(node)] @ spec
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
            | Xsd "attribute", (Begin | Attribute) ->
                let a = parseAttribute(node)
                Attribute, { spec with Attributes = a::spec.Attributes }
            | Xsd "attributeGroup", (Begin | Attribute) ->
                Attribute, node |> notImplementedIn "attributeGroup"
            | Xsd "anyAttribute", (Begin | Attribute | Other) ->
                Other, node |> notImplementedIn "attributeGroup"
            | _ -> node |> notExpectedIn "attributeGroup"
            ) (Begin, { Annotation = ""; Attributes = []; AllowAny = false })
        |> snd

    /// Parses `schema` node contents and completes schemaNode definition details.
    let internal parseSchemaNode schemaNode (node: XElement) =
        node.Elements()
        |> Seq.fold (fun (state, snode) node ->
            match node, state with
            | Xsd "annotation", _ ->
                state, snode
            | Xsd "include", (Begin | Header) ->
                let schloc = node |> reqAttr (xname "schemaLocation")
                Header, { snode with Includes = Uri(schloc)::snode.Includes }
            | Xsd "import", (Begin | Header) ->
                let ns = node |> attrOrDefault (xname "namespace") ""
                let schloc = node |> attr (xname "schemaLocation")
                Header, { snode with Imports = (xns ns, schloc)::snode.Imports }
            | Xsd "redefine", (Begin | Header) ->
                node |> notImplementedIn "schema"
            | Xsd "complexType", _ ->
                let name = node |> reqAttr (xname "name")
                let typ = SchemaType.ComplexType(parseComplexType node)
                snode.Types.Add(xnsname name snode.TargetNamespace.NamespaceName, typ)
                TypeSpec, snode
            | Xsd "element", _ ->
                let element = parseElement node
                match element.Name with
                | None -> failwith "`name` attribute is required if the parent element is the schema element"
                | Some(name) -> snode.Elements.Add(xnsname name snode.TargetNamespace.NamespaceName, element)
                TypeSpec, snode
            | Xsd "simpleType", _ ->
                let name = node |> reqAttr (xname "name")
                let typ = SchemaType.SimpleType(parseSimpleType node)
                snode.Types.Add(xnsname name snode.TargetNamespace.NamespaceName, typ)
                TypeSpec, snode
            | Xsd "attribute", _ ->
                let attribute = parseAttribute(node)
                snode.Attributes.Add(xnsname attribute.Name.Value snode.TargetNamespace.NamespaceName, attribute)
                TypeSpec, snode
            | Xsd "attributeGroup", _ ->
                let ag = node |> parseAttributeGroup
                match SchemaObject<_>.FromNode(node) with
                | Some(Name(name)) ->
                    snode.AttributeGroups.Add(xnsname name.LocalName snode.TargetNamespace.NamespaceName, Definition(ag))
                | Some(Reference(ref)) ->
                    let ns = match ref.NamespaceName with
                                | "" -> snode.TargetNamespace.NamespaceName
                                | x -> x
                    snode.AttributeGroups.Add(xnsname ref.LocalName ns, Definition(ag))
                | _ -> node |> notImplementedIn "schema"
                TypeSpec, snode
            | (Xsd "group" | Xsd "notation"), _ -> node |> notImplementedIn "schema"
            | _ -> node |> notExpectedIn "schema"
            ) (Begin, schemaNode)
        |> snd

    /// Parses all definitions in given schema node.
    let rec private findSchemaNode (schemaLookup: Dictionary<string,SchemaNode>) node =
        let schemaNode = SchemaNode.FromNode(node)
        // Use previously parsed schema if present.
        match schemaLookup.TryGetValue(schemaNode.TargetNamespace.NamespaceName) with
        | false, _ ->
            let schema = node |> parseSchemaNode schemaNode
            schemaLookup.Add(schemaNode.TargetNamespace.NamespaceName, schema)
            // Parse imported schemas also.
            schema.Imports
            |> List.filter (fun (ns, _) -> XmlNamespace.predefined |> List.exists ((=) ns.NamespaceName) |> not)
            |> List.iter (fun (ns, uri) ->
                let document = XDocument.Load(uri |> Option.orDefault(ns.NamespaceName))
                document.Element(xnsname "schema" XmlNamespace.Xsd)
                |> findSchemaNode schemaLookup
                |> ignore)
            schema
        | true, schema -> schema

    /// Parses all type schemas defined and referenced in current WSDL document.
    let parseSchema (definitions: XElement) =
        match definitions.Element(xnsname "types" XmlNamespace.Wsdl) with
        | null -> Map.empty
        | typesNode ->
            let schemaLookup = Dictionary<_,_>()
            typesNode.Elements(xnsname "schema" XmlNamespace.Xsd)
            |> Seq.iter (findSchemaNode schemaLookup >> ignore)
            schemaLookup |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
