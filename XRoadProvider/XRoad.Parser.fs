module internal XRoad.Parser

open System
open System.Collections.Generic
open System.IO
open System.Xml.Linq
open XRoad.Common
open XRoad.ServiceDescription

module XsdSchema =
    let readDimensions name defValue node =
        match node |> attr (XName.Get(name)) with
        | Some("unbounded") -> UInt32.MaxValue
        | Some(x) -> UInt32.Parse(x)
        | _ -> defValue

    let getMinOccurs = readDimensions "minOccurs"
    let getMaxOccurs = readDimensions "maxOccurs"

    let attrTypeValue fparse name defValue node =
        match node |> attr (XName.Get(name)) with
        | Some(v) -> fparse(v)
        | _ -> defValue

    let attrBoolValue = attrTypeValue Boolean.Parse
    let attrIntValue = attrTypeValue Int32.Parse

    let private notexpected (node: XElement) containerName =
        failwithf "Element %A inside %s element was not expected at the current position!" node.Name containerName

    let private notimplemented (node: XElement) containerName =
        failwithf "Element %A inside %s element is not implemented yet." node.Name containerName

    type AttributeUse =
        | Optional
        | Prohibited
        | Required

    type SchemaObject<'T> =
        | Name of XName
        | Reference of XName
        | Definition of 'T

    type ElementSpec =
      { Name: string option
        MinOccurs: uint32
        MaxOccurs: uint32
        IsNillable: bool
        Type: SchemaObject<SchemaType> }

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

    and SimpleTypeRestrictionSpec =
      { Base: XName
        SimpleType: SimpleTypeSpec option
        Content: RestrictionContent list }

    and UnionSpec =
      { MemberTypeNames: XName list
        MemberTypes: SimpleTypeSpec list }

    and SimpleTypeSpec =
        | Restriction of SimpleTypeRestrictionSpec
        //| List
        | Union of UnionSpec

    and ComplexTypeParticle =
        //| Group
        | All of AllSpec
        | Choice of ChoiceSpec
        | Sequence of SequenceSpec

    and SimpleContentRestrictionSpec =
      { Base: XName
        SimpleType: SimpleTypeSpec option
        Content: RestrictionContent list
        Attributes: AttributeSpec list }

    and SimpleContentSpec =
        | Restriction of SimpleContentRestrictionSpec
        | Extension of ExtensionSpec

    and ExtensionSpec =
      { Base: XName
        Content: ComplexTypeContentSpec }

    and ComplexTypeContentSpec =
      { Content: ComplexTypeParticle option
        Attributes: AttributeSpec list }

    and ComplexContentRestrictionSpec =
      { Base: XName
        Content: ComplexTypeContentSpec }

    and ComplexContentSpec =
        | Restriction of ComplexContentRestrictionSpec
        | Extension of ExtensionSpec

    and ComplexTypeContent =
        | SimpleContent of SimpleContentSpec
        | ComplexContent of ComplexContentSpec
        | Particle of ComplexTypeContentSpec

    and ComplexTypeSpec =
      { IsAbstract: bool
        Content: ComplexTypeContent }

    and ChoiceContent =
        | Any
        | Choice of ChoiceSpec
        | Element of ElementSpec
        //| Group of GroupSpec
        | Sequence of SequenceSpec

    and ChoiceSpec =
      { Annotation: string option
        MaxOccurs: uint32
        MinOccurs: uint32
        Content: ChoiceContent list }

    and SequenceContent =
        | Any
        | Element of ElementSpec
        //| Group of GroupDefinition
        | Choice of ChoiceSpec
        | Sequence of SequenceSpec

    and SequenceSpec =
      { MinOccurs: uint32
        MaxOccurs: uint32
        Content: SequenceContent list }

    and AttributeSpec =
      { Name: string option
        RefOrType: SchemaObject<SimpleTypeSpec>
        Use: AttributeUse
        ArrayType: (XName * int) option }

    and AttributeGroupSpec =
      { Annotation: string
        Attributes: AttributeSpec list
        AllowAny: bool }

    and AllSpec =
      { MinOccurs: uint32
        MaxOccurs: uint32
        Elements: ElementSpec list }

    and SchemaType =
        | EmptyType
        | SimpleType of SimpleTypeSpec
        | ComplexType of ComplexTypeSpec

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

    type ParserState =
        | Begin
        | Header
        | Annotation
        | Content
        | Particle
        | Attribute
        | AnyAttribute
        | TypeSpec
        | Other

    let rec parseElement (node: XElement): ElementSpec =
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
                    notimplemented node "element"
                | _ ->
                    notexpected node "element"
                ) (Begin, None)
            |> (fun (_, spec) -> spec |> Option.orDefault(Definition(EmptyType)))
        let elementSpec =
            { Name = None
              MinOccurs = node |> getMinOccurs 1u
              MaxOccurs = node |> getMaxOccurs 1u
              IsNillable = node |> attrBoolValue "nillable" false
              Type = Name(XName.Get("x")) }
        match node |> attr (XName.Get("ref")) with
        | Some refv ->
            match node |> attr (XName.Get("name")) with
            | Some _ -> failwith "Attribute element name and ref attribute cannot be present at the same time."
            | _ -> { elementSpec with Type = Reference(parseXName node refv) }
        | _ ->
            { elementSpec with
                Name = Some(node |> reqAttr (XName.Get("name")))
                Type = match node |> attr (XName.Get("type")) with
                       | Some value -> Name(parseXName node value)
                       | _ -> parseChildElements() }

    and parseUnion (node: XElement): UnionSpec =
        { MemberTypeNames =
            match node |> attr(XName.Get("memberTypes")) with
            | Some(str) ->
                str.Split(' ')
                |> Array.map (fun x ->
                    match x.Split(':') with
                    | [| name |] -> XName.Get(name)
                    | [| ns; name |] -> XName.Get(name, node.GetNamespaceOfPrefix(ns).NamespaceName)
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
                    Content, parseSimpleType(node)::spec
                | _ -> notexpected node "union"
                ) (Begin, [])
            |> snd }

    and parseSimpleType (node: XElement): SimpleTypeSpec =
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
                    Content, notimplemented node "simpleType"
                | _ -> notexpected node "simpleType"
                ) (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element simpleType is expected to contain either restriction, list or union element."

    and parseSimpleTypeRestriction (node: XElement): SimpleTypeRestrictionSpec =
        let typ, content =
            node.Elements()
            |> Seq.fold (fun (state, (typ, content)) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    state, (typ, content)
                | Xsd "simpleType", (Begin | Annotation) ->
                    TypeSpec, notimplemented node "simpleType restriction"
                | Xsd "enumeration", (Begin | Annotation | TypeSpec | Content) ->
                    let value = node |> reqAttr(XName.Get("value"))
                    Content, (typ, Enumeration(value) :: content)
                | Xsd "minLength", (Begin | Annotation | TypeSpec | Content) ->
                    let value = node |> attrIntValue "value" 0
                    Content, (typ, MinLength(value) :: content)
                | Xsd "pattern", (Begin | Annotation | TypeSpec | Content) ->
                    let value = node |> reqAttr(XName.Get("value"))
                    Content, (typ, Pattern(value) :: content)
                | (Xsd "minExclusive" | Xsd "minInclusive" | Xsd "maxExclusive" | Xsd "maxInclusive" | Xsd "totalDigits" | Xsd "fractionDigits" | Xsd "length" | Xsd "minLength" | Xsd "maxLength" | Xsd "whiteSpace"), (Begin | Annotation | TypeSpec | Content) ->
                    Content, notimplemented node "simpleType restriction"
                | (Xsd "attribute" | Xsd "attributeGroup"), (Begin | Annotation | TypeSpec | Content | Attribute) ->
                    Attribute, notimplemented node "simpleType restriction"
                | Xsd "anyAttribute", (Begin | Annotation | TypeSpec | Content | Attribute) ->
                    Other, notimplemented node "simpleType restriction"
                | _ -> notexpected node "simpleType restriction"
                ) (Begin, (None, []))
            |> snd
        { Base = node |> reqAttr (XName.Get("base")) |> parseXName node
          SimpleType = typ
          Content = content }

    and parseComplexType (node: XElement): ComplexTypeSpec =
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
                    notimplemented node "complexType"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.Sequence(parseSequence node)); Attributes = [] }))
                | Xsd "all", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.All(parseAll node)); Attributes = [] }))
                | Xsd "attribute", (Begin | Annotation | Particle | Attribute) ->
                    let attribute = parseAttribute node
                    let content = match spec with
                                  | Some(ComplexTypeContent.Particle(content)) -> { content with Attributes = content.Attributes @ [attribute] }
                                  | None -> { Content = None; Attributes = [attribute] }
                                  | _ -> notexpected node "complexType"
                    Attribute, Some(ComplexTypeContent.Particle(content))
                | Xsd "attributeGroup", (Begin | Annotation | Particle | Attribute) ->
                    notimplemented node "complexType"
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    notimplemented node "complexType"
                | _ ->
                    notexpected node "complexType"
                ) (Begin, None)
            |> snd
            |> Option.get
        { IsAbstract = node |> attrBoolValue "abstract" false
          Content = parseChildElements() }

    and parseChoice (node: XElement): ChoiceSpec =
        let content = List<_>()
        let annotation =
            node.Elements()
            |> Seq.fold (fun (state, notes) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, Some(node.Value)
                | Xsd "any", _ ->
                    content.Add(ChoiceContent.Any)
                    Content, notes
                | Xsd "choice", _ ->
                    content.Add(ChoiceContent.Choice(parseChoice(node)))
                    Content, notes
                | Xsd "element", _ ->
                    content.Add(ChoiceContent.Element(parseElement(node)))
                    Content, notes
                | Xsd "group", _ ->
                    Content, notimplemented node "choice"
                | Xsd "sequence", _ ->
                    content.Add(ChoiceContent.Sequence(parseSequence(node)))
                    Content, notes
                | _ -> notexpected node "choice"
                ) (Begin, None)
            |> snd
        { Annotation = annotation
          MinOccurs = node |> getMinOccurs 1u
          MaxOccurs = node |> getMaxOccurs 1u
          Content = content |> List.ofSeq }

    and parseSequence (node: XElement): SequenceSpec =
        let parseChildElements () =
            (Begin, [])
            |> List.foldBack (fun node (state, spec) ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "any", (Begin | Annotation | Content) ->
                    Content, SequenceContent.Any::spec
                | Xsd "choice", (Begin | Annotation | Content) ->
                    Content, SequenceContent.Choice(parseChoice(node))::spec
                | Xsd "group", (Begin | Annotation | Content) ->
                    Content, notimplemented node "sequence"
                | Xsd "sequence", (Begin | Annotation | Content) ->
                    Content, (SequenceContent.Sequence(parseSequence(node)))::spec
                | Xsd "element", (Begin | Annotation | Content) ->
                    Content, (SequenceContent.Element(parseElement(node)))::spec
                | _ ->
                    notexpected node "sequence"
                ) (node.Elements() |> List.ofSeq)
            |> snd
        { MinOccurs = node |> getMinOccurs 1u
          MaxOccurs = node |> getMaxOccurs 1u
          Content = parseChildElements() }

    and parseAttribute (node: XElement): AttributeSpec =
        let arrayType =
            match node |> attr (XName.Get("arrayType", XmlNamespace.Wsdl)) with
            | Some value ->
                let ns, name = match value.Split(':') with
                                   | [| local |] -> node.GetDefaultNamespace().NamespaceName, local
                                   | [| prefix; local |] -> node.GetNamespaceOfPrefix(prefix).NamespaceName, local
                                   | _ -> failwithf "Invalid array type: %A" value
                match System.Text.RegularExpressions.Regex.Match(name, @"^(\w+)(\[\])+$") with
                | m when m.Success ->
                    Some(XName.Get(m.Groups.[1].Value, ns), m.Groups.[2].Captures.Count)
                | _ -> failwithf "Invalid array type: %A" value
            | _ -> None
        let attrUse = match node |> attrOrDefault (XName.Get("use")) "optional" with
                      | "optional" -> AttributeUse.Optional
                      | "prohibited" -> AttributeUse.Prohibited
                      | "required" -> AttributeUse.Required
                      | x -> failwithf "Invalid attribute use value %s" x
        match node |> attr (XName.Get("ref")) with
        | Some refv ->
            match node |> attr (XName.Get("name")) with
            | Some _ -> failwith "Attribute element name and ref attribute cannot be present at the same time."
            | _ -> { Name = None; RefOrType = Reference(parseXName node refv); ArrayType = arrayType; Use = attrUse }
        | _ ->
            let name = node |> reqAttr (XName.Get("name"))
            match node |> attr (XName.Get("type")) with
            | Some value -> { Name = Some(name); RefOrType = Name(parseXName node value); ArrayType = arrayType; Use = attrUse }
            | _ ->
                node.Elements()
                |> Seq.fold (fun (state, spec) node ->
                    match node, state with
                    | Xsd "annotation", Begin -> Annotation, spec
                    | Xsd "simpleType", (Begin | Annotation) -> TypeSpec, Some(parseSimpleType(node))
                    | _ -> notexpected node "attribute"
                    ) (Begin, None)
                |> (fun (_, typ) -> match typ with
                                    | Some(typ) -> { Name = Some(name); RefOrType = Definition(typ); ArrayType = arrayType; Use = attrUse }
                                    | _ -> failwithf "Attribute element %s type definition is missing." name)

    and parseAll (node: XElement): AllSpec =
        let parseChildElements () =
            (Begin, [])
            |> List.foldBack (fun node (state, spec) ->
                match node, state with
                | Xsd "annotation", Begin -> Annotation, spec
                | Xsd "element", (Begin | Annotation | Content) -> Content, parseElement(node)::spec
                | _ -> notexpected node "all"
                ) (node.Elements() |> List.ofSeq)
            |> snd
        { MinOccurs = node |> getMinOccurs 1u
          MaxOccurs = node |> getMaxOccurs 1u
          Elements = parseChildElements() }

    and parseComplexContent (node: XElement): ComplexContentSpec =
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
                | _ ->
                    notexpected node "complexContent")
                (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element complexContent is expected to contain either restriction or extension element."

    and parseComplexContentRestriction (node: XElement): ComplexContentRestrictionSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexTypeContentSpec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "group", (Begin | Annotation)
                | Xsd "all", (Begin | Annotation)
                | Xsd "choice", (Begin | Annotation) ->
                    notimplemented node "complexContent restriction"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle,  { spec with Content = Some(ComplexTypeParticle.Sequence(parseSequence node)) }
                | Xsd "attribute", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with Attributes = spec.Attributes @ [parseAttribute node] }
                | Xsd "attributeGroup", (Begin | Annotation | Particle | Attribute) ->
                    notimplemented node "complexContent restriction"
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    notimplemented node "complexContent restriction"
                | _ ->
                    notexpected node "complexContent restriction"
                ) (Begin, { Content = None; Attributes = [] })
            |> snd
        { Base = node |> reqAttr (XName.Get("base")) |> parseXName node
          Content = parseChildElements() }

    and parseExtension (node: XElement): ExtensionSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexTypeContentSpec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | (Xsd "group" | Xsd "all" | Xsd "choice"), (Begin | Annotation) ->
                    notimplemented node "extension"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle, { spec with Content = Some(ComplexTypeParticle.Sequence(parseSequence node)) }
                | (Xsd "attribute" | Xsd "attributeGroup"), (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with Attributes = spec.Attributes @ [parseAttribute node] }
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    notimplemented node "extension"
                | _ ->
                    notexpected node "extension"
                ) (Begin, { Content = None; Attributes = [] })
            |> snd
        { Base = node |> reqAttr (XName.Get("base")) |> parseXName node
          Content = parseChildElements() }

    and parseSimpleContent (node: XElement): SimpleContentSpec =
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
                | _ ->
                    notexpected node "simpleContent"
                ) (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element simpleContent is expected to contain either restriction or extension element."

    and parseSimpleContentRestriction (node: XElement): SimpleContentRestrictionSpec =
        notimplemented node "simpleContent"

    and parseAttributeGroup (node: XElement): AttributeGroupSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: AttributeGroupSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, spec
            | Xsd "attribute", (Begin | Attribute) ->
                let a = parseAttribute(node)
                Attribute, { spec with Attributes = a::spec.Attributes }
            | Xsd "attributeGroup", (Begin | Attribute) ->
                Attribute, notimplemented node "attributeGroup"
            | Xsd "anyAttribute", (Begin | Attribute | Other) ->
                Other, notimplemented node "attributeGroup"
            | _ -> notexpected node "attributeGroup"
            ) (Begin, { Annotation = ""; Attributes = []; AllowAny = false })
        |> snd

    let toRefOrName node =
        match node |> attr(XName.Get("name")), node |> attr(XName.Get("ref")) with
        | Some(_), Some(_) -> failwithf "Name and ref attributes cannot both be present (%A)" node.Name.LocalName
        | Some(name), _ -> Some(Name(XName.Get(name)))
        | _, Some(ref) ->
            match ref.Split(':') with
            | [| nm |] -> Some(Reference(XName.Get(nm)))
            | [| pr; nm |] -> Some(Reference(XName.Get(nm, node.GetNamespaceOfPrefix(pr).NamespaceName)))
            | _ -> failwith "wrong ref"
        | _ -> None

    let rec parseSchemaNode(node: XElement, schemaLookup: Dictionary<string,SchemaNode>) =
        let snode = { QualifiedAttributes = node |> isQualified (XName.Get("attributeFormDefault"))
                      QualifiedElements = node |> isQualified (XName.Get("elementFormDefault"))
                      TargetNamespace = node |> attrOrDefault (XName.Get("targetNamespace")) "" |> XNamespace.Get
                      Includes = []
                      Imports = []
                      Attributes = Dictionary<_,_>()
                      Elements = Dictionary<_,_>()
                      Types = Dictionary<_,_>()
                      AttributeGroups = Dictionary<_,_>() }
        match schemaLookup.TryGetValue(snode.TargetNamespace.NamespaceName) with
        | true, schema -> schema
        | _ ->
            let schema =
                node.Elements()
                |> Seq.fold (fun (state, snode) node ->
                    match node, state with
                    | Xsd "annotation", _ ->
                        state, snode
                    | Xsd "include", (Begin | Header) ->
                        let schloc = node |> reqAttr (XName.Get("schemaLocation"))
                        Header, { snode with Includes = Uri(schloc)::snode.Includes }
                    | Xsd "import", (Begin | Header) ->
                        let ns = node |> attrOrDefault (XName.Get("namespace")) ""
                        let schloc = node |> attr (XName.Get("schemaLocation"))
                        Header, { snode with Imports = (XNamespace.Get(ns), schloc)::snode.Imports }
                    | Xsd "redefine", (Begin | Header) ->
                        notimplemented node "schema"
                    | Xsd "complexType", _ ->
                        let name = node |> reqAttr (XName.Get("name"))
                        let typ = SchemaType.ComplexType(parseComplexType node)
                        snode.Types.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), typ)
                        TypeSpec, snode
                    | Xsd "element", _ ->
                        let element = parseElement node
                        match element.Name with
                        | None -> failwith "`name` attribute is required if the parent element is the schema element"
                        | Some(name) -> snode.Elements.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), element)
                        TypeSpec, snode
                    | Xsd "simpleType", _ ->
                        let name = node |> reqAttr (XName.Get("name"))
                        let typ = SchemaType.SimpleType(parseSimpleType node)
                        snode.Types.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), typ)
                        TypeSpec, snode
                    | Xsd "attribute", _ ->
                        let attribute = parseAttribute(node)
                        snode.Attributes.Add(XName.Get(attribute.Name.Value, snode.TargetNamespace.NamespaceName), attribute)
                        TypeSpec, snode
                    | Xsd "attributeGroup", _ ->
                        let ag = node |> parseAttributeGroup
                        match node |> toRefOrName with
                        | Some(Name(name)) ->
                            snode.AttributeGroups.Add(XName.Get(name.LocalName, snode.TargetNamespace.NamespaceName), Definition(ag))
                        | Some(Reference(ref)) ->
                            let ns = match ref.NamespaceName with
                                     | "" -> snode.TargetNamespace.NamespaceName
                                     | x -> x
                            snode.AttributeGroups.Add(XName.Get(ref.LocalName, ns), Definition(ag))
                        | _ -> notimplemented node "schema"
                        TypeSpec, snode
                    | (Xsd "group" | Xsd "notation"), _ ->
                        notimplemented node "schema"
                    | _ ->
                        notexpected node "schema"
                    ) (Begin, snode)
                |> snd
            schemaLookup.Add(snode.TargetNamespace.NamespaceName, schema)
            schema.Imports
            |> List.filter (fun (ns, _) -> XmlNamespace.predefined |> List.exists (fun pdns -> ns.NamespaceName = pdns) |> not)
            |> List.iter (fun (ns, uri) ->
                let document = XDocument.Load(uri |> Option.orDefault(ns.NamespaceName))
                let schemaNode = document.Element(XName.Get("schema", XmlNamespace.Xsd))
                parseSchemaNode(schemaNode, schemaLookup) |> ignore)
            schema

    let parseSchema (definitions: XElement) =
        match definitions.Element(XName.Get("types", XmlNamespace.Wsdl)) with
        | null -> Map.empty
        | typesNode ->
            let schemaLookup = Dictionary<_,_>()
            let parseSchemaNode' n = parseSchemaNode(n, schemaLookup) |> ignore
            typesNode.Elements(XName.Get("schema", XmlNamespace.Xsd)) |> Seq.iter parseSchemaNode'
            schemaLookup |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

