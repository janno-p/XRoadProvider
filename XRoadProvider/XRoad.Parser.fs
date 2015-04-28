module internal XRoad.Parser

open System
open System.Collections.Generic
open System.IO
open System.Xml.Linq

[<AutoOpen>]
module Option =
    let orDefault value opt =
        opt |> Option.fold (fun _ t -> t) value

module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEncoding = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnvelope = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"
    let predefined = [ Http; Mime; Soap; SoapEncoding; SoapEnvelope; Wsdl; Xml; Xsd; Xsi ]

let resolveUri uri =
    match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
    | true -> uri
    | _ ->
        let fullPath = (new FileInfo(uri)).FullName
        match File.Exists(fullPath) with
        | true -> fullPath
        | _ -> failwith (sprintf "Cannot resolve url location `%s`" uri)

type XmlReference =
    | SchemaElement of XName
    | SchemaType of XName
    with member this.XName with get() = match this with SchemaElement(name) | SchemaType(name) -> name

type MessagePart =
  { Name: string
    Reference: XmlReference }

type OperationBody =
  { Namespace: string
    Parts: MessagePart list }

type OperationMessage =
  { Name: XName
    Body: OperationBody
    Header: MessagePart list
    MultipartContent: MessagePart list }
    static member Create(name) = { Name = name; Body = { Namespace = ""; Parts = [] }; Header = []; MultipartContent = [] }

type OperationStyle = RpcEncoded | DocLiteral

type Operation =
  { Name: XName
    Version: string option
    Style: OperationStyle
    Request: OperationMessage
    Response: OperationMessage
    Documentation: IDictionary<string,string> }

type PortBinding =
  { Name: string
    Address: string
    Producer: string
    Documentation: IDictionary<string,string>
    Operations: Operation list
    Style: OperationStyle }

type Service =
  { Name: string
    Ports: PortBinding list }

let attr (name: XName) (element: XElement) =
    match element.Attribute(name) with
    | null -> None
    | attr -> Some attr.Value

let attrOrDefault name value element =
    element |> attr name |> Option.orDefault value

let reqAttr (name: XName) (element: XElement) =
    match element.Attribute name with
    | null -> failwithf "Element %A attribute %A is required!" element.Name name
    | attr -> attr.Value

let parseXName (element: XElement) (qualifiedName: string) =
    match qualifiedName.Split(':') with
    | [|name|] -> XName.Get(name, element.GetDefaultNamespace().NamespaceName)
    | [|prefix; name|] -> XName.Get(name, element.GetNamespaceOfPrefix(prefix).NamespaceName)
    | _ -> failwithf "Invalid qualified name string %s" qualifiedName

let parseParamName name (element: XElement) =
    let paramElement = element.Element(XName.Get(name, XmlNamespace.Wsdl))
    paramElement
    |> reqAttr (XName.Get("message"))
    |> parseXName paramElement

let parseParam (definitions: XElement) (name: XName) =
    let targetNamespace = definitions |> attrOrDefault (XName.Get("targetNamespace")) ""
    if name.NamespaceName <> targetNamespace then
        failwithf "External messages are not supported yet! [%O]" name
    definitions.Elements(XName.Get("message", XmlNamespace.Wsdl))
    |> Seq.find (fun el -> (el |> reqAttr (XName.Get("name"))) = name.LocalName)

let readLanguages (element: XElement) =
    element.Elements(XName.Get("title", XmlNamespace.XRoad))
    |> Seq.fold (fun (doc: IDictionary<string,string>) el ->
        let lang = match el |> attr (XName.Get("lang", XmlNamespace.Xml)) with
                   | Some lang -> lang
                   | _ -> "en"
        doc.[lang] <- el.Value
        doc) (Dictionary<_,_>() :> IDictionary<_,_>)

let readDocumentation (element: XElement): IDictionary<string,string> =
    match element.Element(XName.Get("documentation", XmlNamespace.Wsdl)) with
    | null -> upcast Dictionary<_,_>()
    | element -> readLanguages element

let parseAbstractParts msgName (abstractDef: XElement) =
    abstractDef.Elements(XName.Get("part", XmlNamespace.Wsdl))
    |> Seq.map (fun elem ->
        let name = elem |> attrOrDefault (XName.Get("name")) ""
        match (elem |> attr (XName.Get("element"))), (elem |> attr (XName.Get("type"))) with
        | Some el, _ -> name, SchemaElement(parseXName elem el)
        | _, Some tp -> name, SchemaType(parseXName elem tp)
        | _ -> failwithf "Unknown element or type for message %s part %s" msgName name)
    |> Map.ofSeq

let parseSoapBody msgName ns (style: OperationStyle) (abstractParts: Map<string,XmlReference>) (elem: XElement) (opmsg: OperationMessage) =
    let ns =
        match elem |> reqAttr(XName.Get("use")), style with
        | "literal", RpcEncoded -> failwith "Invalid use value 'literal' for RPC style, only 'encoded' is allowed."
        | "literal", DocLiteral -> ns
        | "encoded", DocLiteral -> failwith "Invalid use value 'encoded' for RPC style, only 'literal' is allowed."
        | "encoded", RpcEncoded -> elem |> reqAttr(XName.Get("namespace"))
        | u, _ -> failwithf "Unexpected use/style combination: %A/%A" u style
    let opmsg =
        match elem |> attr (XName.Get("parts")) with
        | Some value ->
            value.Split(' ')
            |> Array.fold (fun om partName ->
                match abstractParts.TryFind partName with
                | Some(part) ->
                    { om with
                        Body =
                            if om.Body.Parts |> List.exists (fun x -> x.Name = partName) then om.Body
                            else { om.Body with Parts = { Name = partName; Reference = part } :: om.Body.Parts } }
                | None -> failwithf "Message %s does not contain part %s" msgName partName
                ) opmsg
        | _ -> opmsg
    { opmsg with Body = { opmsg.Body with Namespace = ns } }

let parseSoapHeader (style: OperationStyle) (definitions: XElement) (elem: XElement) (opmsg: OperationMessage) =
    let messageName = elem |> reqAttr (XName.Get("message")) |> parseXName elem
    let partName = elem |> reqAttr (XName.Get("part"))
    match elem |> reqAttr (XName.Get("use")), style with
    | "literal", RpcEncoded ->
        failwith "Invalid use value literal for RPC style, only encoded is allowed."
    | "encoded", DocLiteral ->
        failwith "Invalid use value encoded for document style, only literal is allowed."
    | _ -> ()
    let message = parseParam definitions messageName
    let parts = message |> parseAbstractParts messageName.LocalName
    match parts.TryFind partName with
    | Some(value) -> { opmsg with Header = { Name = partName; Reference = value } :: opmsg.Header }
    | None -> failwithf "Message %s does not contain part %s" messageName.LocalName partName

let parseOperationMessage (style: OperationStyle) (binding: XElement) (abstractDef: XElement) ns =
    let msgName = abstractDef |> reqAttr (XName.Get("name"))
    let definitions = binding.Parent.Parent.Parent
    let abstractParts = abstractDef |> parseAbstractParts msgName
    let parseSoapBody' = parseSoapBody msgName ns style abstractParts
    let parseSoapHeader' = parseSoapHeader style definitions
    let operationMessage =
        binding.Elements()
        |> Seq.fold (fun opmsg elem ->
            match elem.Name.NamespaceName, elem.Name.LocalName with
            | XmlNamespace.Soap, "body" -> opmsg |> parseSoapBody' elem
            | XmlNamespace.Soap, "header" -> opmsg |> parseSoapHeader' elem
            | XmlNamespace.Mime, "multipartRelated" ->
                elem.Elements(XName.Get("part", XmlNamespace.Mime))
                |> Seq.fold (fun opmsg elem ->
                    elem.Elements()
                    |> Seq.fold (fun opmsg elem ->
                        match elem.Name.NamespaceName, elem.Name.LocalName with
                        | XmlNamespace.Soap, "body" -> opmsg |> parseSoapBody' elem
                        | XmlNamespace.Soap, "header" -> opmsg |> parseSoapHeader' elem
                        | XmlNamespace.Mime, "content" ->
                            let partName = elem |> reqAttr (XName.Get("part"))
                            match abstractParts.TryFind partName with
                            | Some(value) -> { opmsg with MultipartContent = { Name = partName; Reference = value } :: opmsg.MultipartContent }
                            | None -> failwithf "Message %s does not contain part %s" msgName partName
                        | _ -> opmsg) opmsg) opmsg
            | _ -> opmsg) (OperationMessage.Create(XName.Get(msgName, ns)))
    abstractParts
    |> Seq.fold (fun opmsg p ->
        if opmsg.Body.Parts |> List.exists (fun x -> x.Name = p.Key) then opmsg
        elif opmsg.MultipartContent |> List.exists (fun x -> x.Name = p.Key) then opmsg
        else { opmsg with Body = { opmsg.Body with Parts = { Name = p.Key; Reference = p.Value } :: opmsg.Body.Parts } }
        ) operationMessage

let validateOperationStyle styleValue style =
    match styleValue with
    | "document" -> match style with
                    | RpcEncoded -> failwith "Binding style `document` doesn't match legacy message format."
                    | DocLiteral -> ()
    | "rpc" -> match style with
               | RpcEncoded -> ()
               | DocLiteral -> failwith "Binding style `rpc` doesn't match new message format."
    | x -> failwithf "Unknown SOAP binding style %s" x

let parseOperation (operation: XElement) (portType: XElement) (definitions: XElement) (style: OperationStyle) ns =
    let name = operation |> reqAttr (XName.Get("name"))
    let version =
        let ns = match style with | RpcEncoded -> XmlNamespace.Xtee | DocLiteral -> XmlNamespace.XRoad
        match operation.Element(XName.Get("version", ns)) with
        | null -> None
        | el -> Some el.Value
    match operation.Element(XName.Get("operation", XmlNamespace.Soap)) with
    | null -> ()
    | soapOperation ->
        let styleValue = soapOperation |> attrOrDefault (XName.Get("style")) (match style with | DocLiteral -> "document" | RpcEncoded -> "rpc")
        validateOperationStyle styleValue style
    let abstractDesc = portType.Elements(XName.Get("operation", XmlNamespace.Wsdl))
                       |> Seq.find (fun op -> (op |> reqAttr (XName.Get("name"))) = name)
    let paramInput = abstractDesc |> parseParamName "input" |> parseParam definitions
    let paramOutput = abstractDesc |> parseParamName "output" |> parseParam definitions
    { Name = XName.Get(name, ns)
      Version = version
      Style = style
      Request = parseOperationMessage style (operation.Element(XName.Get("input", XmlNamespace.Wsdl))) paramInput ns
      Response = parseOperationMessage style (operation.Element(XName.Get("output", XmlNamespace.Wsdl))) paramOutput ns
      Documentation = readDocumentation abstractDesc }

let parseBinding (definitions: XElement) (bindingName: XName) (portBinding: PortBinding) =
    let targetNamespace = definitions |> attrOrDefault (XName.Get("targetNamespace")) ""
    if bindingName.NamespaceName <> targetNamespace then
        failwithf "External namespaces are not yet supported! Given %s." bindingName.NamespaceName
    let binding = definitions.Elements(XName.Get("binding", XmlNamespace.Wsdl))
                  |> Seq.find (fun el -> (el |> reqAttr (XName.Get("name"))) = bindingName.LocalName)
    let portTypeName = binding |> reqAttr (XName.Get("type")) |> parseXName binding
    if portTypeName.NamespaceName <> targetNamespace then
        failwithf "External namespaces are not yet supported! Given %s." portTypeName.NamespaceName
    let portType = definitions.Elements(XName.Get("portType", XmlNamespace.Wsdl))
                   |> Seq.find (fun el -> (el |> reqAttr (XName.Get("name"))) = portTypeName.LocalName)
    let soapBinding = binding.Element(XName.Get("binding", XmlNamespace.Soap))
    validateOperationStyle (soapBinding |> attrOrDefault (XName.Get("style")) "document") portBinding.Style
    let transport = soapBinding |> attrOrDefault (XName.Get("transport")) ""
    if transport <> XmlNamespace.Http then
        failwithf "Only HTTP transport is allowed. Specified %s" transport
    let operations =
        binding.Elements(XName.Get("operation", XmlNamespace.Wsdl))
        |> Seq.map (fun op -> parseOperation op portType definitions portBinding.Style portTypeName.NamespaceName)
        |> List.ofSeq
    { portBinding with Operations = operations }

let parseServices (definitions: XElement) =
    definitions.Elements(XName.Get("service", XmlNamespace.Wsdl))
    |> Seq.map (fun service ->
        let name =  service |> reqAttr (XName.Get("name"))
        let ports =
            service.Elements(XName.Get("port", XmlNamespace.Wsdl))
            |> Seq.choose (fun servicePort ->
                let name = servicePort |> reqAttr (XName.Get("name"))
                let binding = servicePort |> reqAttr (XName.Get("binding")) |> parseXName servicePort
                let address = match servicePort.Element(XName.Get("address", XmlNamespace.Soap)) with
                              | null -> ""
                              | elem -> elem |> reqAttr (XName.Get("location"))
                let producer =
                    let current = servicePort.Element (XName.Get("address", XmlNamespace.XRoad))
                    let legacy = servicePort.Element (XName.Get("address", XmlNamespace.Xtee))
                    match current, legacy with
                    | null, null -> None
                    | elem, null
                    | null, elem ->
                        let name = elem |> attr (XName.Get("producer")) |> Option.orDefault ""
                        let style = if elem.Name.NamespaceName = XmlNamespace.XRoad then DocLiteral
                                    else RpcEncoded
                        Some(name, style)
                    | _ -> failwith "Mixing legacy operations with new message format is not supported."
                producer |> Option.map (fun (producer, style) ->
                    let doc = readLanguages servicePort
                    let bindingSpec = { Name = name
                                        Address = address
                                        Producer = producer
                                        Documentation = doc
                                        Operations = []
                                        Style = style }
                    parseBinding definitions binding bindingSpec))
        { Name = name; Ports = ports |> List.ofSeq })
    |> List.ofSeq

module XsdSchema =
    let (|Xsd|_|) (element: XElement) =
        match element.Name.NamespaceName with
        | XmlNamespace.Xsd -> Some element.Name.LocalName
        | _ -> None

    let (|XmlName|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.Xml -> Some name.LocalName
        | _ -> None

    let (|XsdType|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.Xsd -> Some name.LocalName
        | _ -> None

    let (|XrdName|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.XRoad -> Some name.LocalName
        | _ -> None

    let (|XteeName|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.Xtee -> Some name.LocalName
        | _ -> None

    let (|SoapEncType|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.SoapEncoding -> Some name.LocalName
        | _ -> None

    let isQualified attrName node =
        match node |> attrOrDefault attrName "unqualified" with
        | "qualified" -> true
        | "unqualified" -> false
        | x -> failwithf "Unknown %s value '%s'" attrName.LocalName x

    type TypeNode =
      { X: string }

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

    // XRoad RPC/Encoded spec
    // Kui teenus esitatakse MIME-konteinerina, siis manustena saadetakse parajasti kõik
    // teenuse sisendis olevad skalaarsed elemendid, mille tüüp on xsd:base64Binary või
    // xsd:hexBinary.

    let mapPrimitiveType = function
        | XsdType "anyURI" -> Some typeof<string>
        | XsdType "hexBinary" -> Some typeof<byte[]>
        | XsdType "base64Binary" -> Some typeof<byte[]>
        | XsdType "boolean" -> Some typeof<bool>
        | XsdType "date" -> Some typeof<DateTime>
        | XsdType "dateTime" -> Some typeof<DateTime>
        | XsdType "decimal" -> Some typeof<decimal>
        | XsdType "int" -> Some typeof<int>
        | XsdType "integer" -> Some typeof<bigint>
        | XsdType "long" -> Some typeof<int64>
        | XsdType "string" -> Some typeof<string>
        | XsdType "NMTOKEN" -> Some typeof<string>
        | XsdType name -> failwithf "Unmapped XSD type %s" name
        | SoapEncType "base64Binary" -> Some typeof<byte[]>
        | SoapEncType name -> failwithf "Unmapped SOAP-ENC type %s" name
        | _ -> None

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

type Schema =
  { TypeSchemas: Map<string,XsdSchema.SchemaNode>
    Services: Service list }

let readSchema (uri: string) =
    let document = XDocument.Load(uri)
    let definitionsNode = document.Element(XName.Get("definitions", XmlNamespace.Wsdl))
    { Services = definitionsNode |> parseServices
      TypeSchemas = definitionsNode |> XsdSchema.parseSchema }

let (|IsXteeHeader|_|) (part: MessagePart) =
    match part.Reference with
    | SchemaElement name -> match name with
                            | XsdSchema.XteeName "asutus"
                            | XsdSchema.XteeName "andmekogu"
                            | XsdSchema.XteeName "isikukood"
                            | XsdSchema.XteeName "ametnik"
                            | XsdSchema.XteeName "id"
                            | XsdSchema.XteeName "nimi"
                            | XsdSchema.XteeName "toimik"
                            | XsdSchema.XteeName "allasutus"
                            | XsdSchema.XteeName "amet"
                            | XsdSchema.XteeName "ametniknimi"
                            | XsdSchema.XteeName "asynkroonne"
                            | XsdSchema.XteeName "autentija"
                            | XsdSchema.XteeName "makstud"
                            | XsdSchema.XteeName "salastada"
                            | XsdSchema.XteeName "salastada_sertifikaadiga"
                            | XsdSchema.XteeName "salastatud"
                            | XsdSchema.XteeName "salastatud_sertifikaadiga" -> Some(name)
                            | _ -> None
    | _ -> None

let (|IsXRoadHeader|_|) (part: MessagePart) =
    match part.Reference with
    | SchemaElement name -> match name with
                            | XsdSchema.XrdName "consumer"
                            | XsdSchema.XrdName "producer"
                            | XsdSchema.XrdName "userId"
                            | XsdSchema.XrdName "id"
                            | XsdSchema.XrdName "service"
                            | XsdSchema.XrdName "issue"
                            | XsdSchema.XrdName "unit"
                            | XsdSchema.XrdName "position"
                            | XsdSchema.XrdName "userName"
                            | XsdSchema.XrdName "async"
                            | XsdSchema.XrdName "authenticator"
                            | XsdSchema.XrdName "paid"
                            | XsdSchema.XrdName "encrypt"
                            | XsdSchema.XrdName "encryptCert"
                            | XsdSchema.XrdName "encrypted"
                            | XsdSchema.XrdName "encryptedCert" -> Some(name)
                            | _ -> None
    | _ -> None
