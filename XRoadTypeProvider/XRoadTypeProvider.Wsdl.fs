module XRoadTypeProvider.Wsdl

open System
open System.Collections.Generic
open System.IO
open System.Xml.Linq

module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnvelope = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"

module Option =
    let orDefault value opt =
        opt |> Option.fold (fun s t -> t) value

let private mapXrdType = function
    | "faultCode"
    | "faultString" -> typeof<string>
    | n             -> failwithf "Unmapped XRD type %s" n

let private mapXrdElementType = function
    | "async" -> typeof<bool>
    | "address"
    | "authenticator"
    | "consumer"
    | "encode"
    | "id"
    | "issue"
    | "nocontent"
    | "notes"
    | "position"
    | "producer"
    | "ref"
    | "requirecontent"
    | "service"
    | "technotes"
    | "title"
    | "unit"
    | "userId"
    | "userName"
    | "version"
    | "wildcard" -> typeof<string>
    | "listMethods"
    | "listMethodsResponse"
    | "testSystem"
    | "testSystemResponse"
    | "loadClassification"
    | "loadClassificationResponse"
    | "userAllowedMethods"
    | "userAllowedMethodsResponse" -> typeof<obj>
    // HACK: these are really complexTypes
    | "unitRepresent"
    | "unitRepresentResponse"
    | "unitValid"
    | "unitValidResponse" -> typeof<obj>
    | n -> failwithf "Unmapped XRD element type %s" n

let mapXteeElementType = function
    | "asynkroonne" -> typeof<bool>
    | "allasutus"
    | "amet"
    | "ametnik"
    | "ametniknimi"
    | "andmekogu"
    | "asutus"
    | "autentija"
    | "id"
    | "isikukood"
    | "nimi"
    | "nocontent"
    | "notes"
    | "ref"
    | "requirecontent"
    | "title"
    | "technotes"
    | "toimik"
    | "version"
    | "wildcard" -> typeof<string>
    | "address"
    | "complex" -> typeof<obj>
    | x -> failwithf "Unmapped XRD element type %s" x

let resolveType (name: XName) =
    match name.NamespaceName with
    | XmlNamespace.XRoad -> mapXrdType name.LocalName
    | _ -> failwithf "Unmapped type name %O" name

let resolveElementType (name: XName) tns =
    match name.NamespaceName with
    | XmlNamespace.XRoad -> mapXrdElementType name.LocalName
    | XmlNamespace.Xtee -> mapXteeElementType name.LocalName
    | ns when ns = tns -> typeof<obj>
    | _ -> failwithf "Unmapped element name %O" name

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

type MessagePart =
  { Name: string
    Reference: XmlReference }

type OperationMessage =
  { Body: MessagePart list
    Header: MessagePart list
    MultipartContent: MessagePart list }
    static member Empty with get() = { Body = []; Header = []; MultipartContent = [] }

type Operation =
  { Name: string
    Version: string option
    Style: XRoad.XRoadBindingStyle
    Request: OperationMessage
    Response: OperationMessage
    Documentation: IDictionary<string,string> }

type PortBinding =
  { Name: string
    Address: string
    Producer: string
    Documentation: IDictionary<string,string>
    Operations: Operation list
    Style: XRoad.XRoadBindingStyle }
    static member Empty with get() = { Name = ""
                                       Address = ""
                                       Producer = ""
                                       Documentation = Dictionary<_,_>()
                                       Operations = []
                                       Style = XRoad.XRoadBindingStyle.DocumentLiteral }

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
    |> Seq.fold (fun (d: Dictionary<string,XmlReference>) (k, v) -> d.[k] <- v; d) (Dictionary<_,_>())

let parseSoapBody msgName (abstractParts: IDictionary<string,XmlReference>) (elem: XElement) (opmsg: OperationMessage) =
    match elem |> attr (XName.Get("parts")) with
    | Some value ->
        value.Split(' ')
        |> Array.fold (fun om partName ->
            match abstractParts.TryGetValue partName with
            | true, part ->
                abstractParts.Remove(partName) |> ignore
                { om with Body = { Name = partName; Reference = part } :: om.Body }
            | _ -> failwithf "Message %s does not contain part %s" msgName partName
            ) opmsg
    | _ -> opmsg

let parseSoapHeader (style: XRoad.XRoadBindingStyle) (definitions: XElement) (elem: XElement) (opmsg: OperationMessage) =
    let messageName = elem |> reqAttr (XName.Get("message")) |> parseXName elem
    let partName = elem |> reqAttr (XName.Get("part"))
    match elem |> reqAttr (XName.Get("use")), style with
    | "literal", XRoad.XRoadBindingStyle.RpcEncoded ->
        failwith "Invalid use value literal for RPC style, only encoded is allowed."
    | "encoded", XRoad.XRoadBindingStyle.DocumentLiteral ->
        failwith "Invalid use value encoded for document style, only literal is allowed."
    | _ -> ()
    let message = parseParam definitions messageName
    let parts = message |> parseAbstractParts messageName.LocalName
    match parts.TryGetValue partName with
    | true, value -> { opmsg with Header = { Name = partName; Reference = value } :: opmsg.Header }
    | _ -> failwithf "Message %s does not contain part %s" messageName.LocalName partName

let parseOperationMessage (style: XRoad.XRoadBindingStyle) (binding: XElement) (abstractDef: XElement) =
    let msgName = abstractDef |> reqAttr (XName.Get("name"))
    let definitions = binding.Parent.Parent.Parent
    let abstractParts = abstractDef |> parseAbstractParts msgName
    let parseSoapBody' = parseSoapBody msgName abstractParts
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
                            match abstractParts.TryGetValue partName with
                            | true, value ->
                                abstractParts.Remove(partName) |> ignore
                                { opmsg with MultipartContent = { Name = partName; Reference = value } :: opmsg.MultipartContent }
                            | _ -> failwithf "Message %s does not contain part %s" msgName partName
                        | _ -> opmsg) opmsg) opmsg
            | _ -> opmsg) OperationMessage.Empty
    abstractParts |> Seq.fold (fun opmsg p -> { opmsg with Body = { Name = p.Key; Reference = p.Value } :: opmsg.Body }) operationMessage

let parseOperation (operation: XElement) (portType: XElement) (definitions: XElement) (style: XRoad.XRoadBindingStyle) =
    let name = operation |> reqAttr (XName.Get("name"))
    let version = match operation.Element(XName.Get("version", XmlNamespace.XRoad)) with
                  | null -> None
                  | el -> Some el.Value
    let bindingStyle =
        match operation.Element(XName.Get("operation", XmlNamespace.Soap)) with
        | null -> style
        | soapOperation -> match soapOperation |> attr (XName.Get("style")) with
                           | Some "document" -> XRoad.XRoadBindingStyle.DocumentLiteral
                           | Some "rpc" -> XRoad.XRoadBindingStyle.RpcEncoded
                           | Some x -> failwithf "Unknown SOAP binding style %s" x
                           | _ -> style
    let abstractDesc = portType.Elements(XName.Get("operation", XmlNamespace.Wsdl))
                       |> Seq.find (fun op -> (op |> reqAttr (XName.Get("name"))) = name)
    let paramInput = abstractDesc |> parseParamName "input" |> parseParam definitions
    let paramOutput = abstractDesc |> parseParamName "output" |> parseParam definitions
    { Name = name
      Version = version
      Style = bindingStyle
      Request = parseOperationMessage bindingStyle (operation.Element(XName.Get("input", XmlNamespace.Wsdl))) paramInput
      Response = parseOperationMessage bindingStyle (operation.Element(XName.Get("output", XmlNamespace.Wsdl))) paramOutput
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
    let bindingStyle = match soapBinding |> attrOrDefault (XName.Get("style")) "document" with
                       | "document" -> XRoad.XRoadBindingStyle.DocumentLiteral
                       | "rpc" -> XRoad.XRoadBindingStyle.RpcEncoded
                       | x -> failwithf "Unknown SOAP binding style %s" x
    let transport = soapBinding |> attrOrDefault (XName.Get("transport")) ""
    if transport <> XmlNamespace.Http then
        failwithf "Only HTTP transport is allowed. Specified %s" transport
    let operations =
        binding.Elements(XName.Get("operation", XmlNamespace.Wsdl))
        |> Seq.map (fun op -> parseOperation op portType definitions bindingStyle)
        |> List.ofSeq
    { portBinding with Operations = operations; Style = bindingStyle }

let parseServices (definitions: XElement) =
    definitions.Elements(XName.Get("service", XmlNamespace.Wsdl))
    |> Seq.map (fun service ->
        let name =  service |> reqAttr (XName.Get("name"))
        let ports =
            service.Elements(XName.Get("port", XmlNamespace.Wsdl))
            |> Seq.map (fun servicePort ->
                let name = servicePort |> reqAttr (XName.Get("name"))
                let binding = servicePort |> reqAttr (XName.Get("binding")) |> parseXName servicePort
                let address = match servicePort.Element(XName.Get("address", XmlNamespace.Soap)) with
                              | null -> ""
                              | elem -> elem |> reqAttr (XName.Get("location"))
                let producer = match servicePort.Element (XName.Get("address", XmlNamespace.XRoad)) with
                               | null -> ""
                               | elem -> match elem |> attr (XName.Get("producer")) with | None -> "" | Some v -> v
                let portBinding = PortBinding.Empty
                let doc = readLanguages servicePort
                { portBinding with Name = name
                                   Address = address
                                   Producer = producer
                                   Documentation = doc }
                |> parseBinding definitions binding)
        { Name = name; Ports = ports |> List.ofSeq })
    |> List.ofSeq

module XsdSchema =
    let (|Xsd|_|) (element: XElement) =
        match element.Name.NamespaceName with
        | XmlNamespace.Xsd -> Some element.Name.LocalName
        | _ -> None

    let (|XsdType|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.Xsd -> Some name.LocalName
        | _ -> None

    let (|XrdType|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.XRoad -> Some name.LocalName
        | _ -> None

    type TypeDefinition =
      { Attributes: (string * SchemaType) list
        Properties: (string * SchemaType) list }

    and SchemaType =
        | TypeReference of XName
        | TypeDefinition of TypeDefinition

    type SchemaNode =
      { QualifiedAttributes: bool
        QualifiedElements: bool
        TargetNamespace: XNamespace
        Includes: Uri list
        Imports: (XNamespace * Uri option) list
        Elements: IDictionary<XName,SchemaType>
        Types: IDictionary<XName,TypeDefinition> }

    let isQualified attrName node =
        match node |> attrOrDefault attrName "unqualified" with
        | "qualified" -> true
        | "unqualified" -> false
        | x -> failwithf "Unknown %s value '%s'" attrName.LocalName x

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

    type TypeNode =
      { X: string }

    let mapPrimitiveType = function
        | XsdType "integer" -> Some typeof<int>
        | XsdType "base64Binary" -> Some typeof<byte[]>
        | XsdType "string" -> Some typeof<string>
        | XsdType name -> failwithf "Unmapped XSD type %s" name
        | XrdType "faultCode"
        | XrdType "faultString" -> Some typeof<string>
        | XrdType "jpg" -> Some typeof<byte[]>
        | XrdType "maakond" -> Some typeof<string>
        | XrdType name -> failwithf "Unmapped XRD type %s" name
        | _ -> None

    let rec parseElement (node: XElement) =
        (*<element  id=ID
                    name=NCName
                    ref=QName
                    type=QName
                    substitutionGroup=QName
                    default=string
                    fixed=string
                    form=qualified|unqualified
                    maxOccurs=nonNegativeInteger|unbounded
                    minOccurs=nonNegativeInteger
                    nillable=true|false
                    abstract=true|false
                    block=(#all|list of (extension|restriction))
                    final=(#all|list of (extension|restriction))
                    any attributes>
            annotation?,(simpleType|complexType)?,(unique|key|keyref)*
          </element>*)
        let rec parseInnerNodes (part: ParserState) (typeDef: TypeDefinition) (nodes: XElement list) =
            match part, nodes with
            | _, [] -> typeDef
            | Begin, ((Xsd "annotation") as node)::nodes -> // Ignored
                parseInnerNodes Annotation typeDef nodes
            | (Begin | Annotation), ((Xsd "simpleType") as node)::nodes ->
                failwith "SimpleType elements for Element are not implemented yet!"
            | (Begin | Annotation), ((Xsd "complexType") as node)::nodes ->
                parseInnerNodes TypeSpec (parseComplexType node) nodes
            | (Begin | Annotation | TypeSpec | Other), ((Xsd "unique" | Xsd "key" | Xsd "keyref") as node)::nodes ->
                failwith "Other elements for Element are not implemented yet!"
            | _, node::_ ->
                failwithf "Element %s was not expected at the current position." node.Name.LocalName
        let name = node |> reqAttr (XName.Get("name"))
        match node |> attr (XName.Get("type")) with
        | Some value -> (name, TypeReference(parseXName node value))
        | _ ->
            let typeDef = { Attributes = []; Properties = [] }
            (name, node.Elements() |> List.ofSeq |> parseInnerNodes Begin typeDef |> TypeDefinition)

    and parseComplexType (node: XElement) =
        let isAbstract = match node |> attrOrDefault (XName.Get("abstract")) "false" with
                         | "true" -> true
                         | "false" -> false
                         | x -> failwithf "Invalid value %s for complexType::abstract attribute" x
        let rec parseInnerNodes (part: ParserState) (typeDef: TypeDefinition) (nodes: XElement list) =
            match part, nodes with
            | _, [] ->
                typeDef
            | Begin, (Xsd "annotation" as node)::nodes ->
                parseInnerNodes Annotation typeDef nodes
            | (Begin | Annotation), (Xsd "simpleContent" as node)::nodes ->
                failwithf "Element %A in complexType element is not implemented yet!" node.Name
            | (Begin | Annotation), (Xsd "complexContent" as node)::nodes ->
                parseComplexContent node
                failwithf "Element %A in complexType element is not implemented yet!" node.Name
            | (Begin | Annotation), ((Xsd "group" | Xsd "choice") as node)::nodes ->
                failwithf "Element %A in complexType element is not implemented yet!" node.Name
            | (Begin | Annotation), ((Xsd "sequence") as node)::nodes ->
                let properties = parseSequence node
                parseInnerNodes Particle { typeDef with Properties = typeDef.Properties @ properties } nodes
            | (Begin | Annotation), ((Xsd "all") as node)::nodes ->
                let properties = parseAll node
                parseInnerNodes Particle { typeDef with Properties = typeDef.Properties @ properties } nodes
            | (Begin | Annotation | Particle | Attribute), (Xsd "attribute" as node)::nodes ->
                let attribute = parseAttribute node
                parseInnerNodes Attribute { typeDef with Attributes = typeDef.Attributes @ [attribute] } nodes
            | (Begin | Annotation | Particle | Attribute), (Xsd "attributeGroup" as node)::nodes ->
                failwithf "Element %A in complexType element is not implemented yet!" node.Name
            | (Begin | Annotation | Particle | Attribute), ((Xsd "anyAttribute") as node)::nodes ->
                failwithf "Element %A in complexType element is not implemented yet!" node.Name
            | _, node::_ ->
                failwithf "Element %A in complexType element was not expected at the current position!" node.Name
        node.Elements() |> List.ofSeq |> parseInnerNodes Begin { Attributes = []; Properties = [] }

    and parseSequence (node: XElement): (string * SchemaType) list =
        (*<sequence id=ID
                    maxOccurs=nonNegativeInteger|unbounded
                    minOccurs=nonNegativeInteger
                    any attributes>
            (annotation?,(element|group|choice|sequence|any)* )
          </sequence>*)
        let minOccurs = match node |> attrOrDefault (XName.Get("minOccurs")) "1" with
                        | "unbounded" -> None
                        | x -> Some(Int64.Parse(x))
        let maxOccurs = match node |> attrOrDefault (XName.Get("maxOccurs")) "1" with
                        | "unbounded" -> None
                        | x -> Some(Int64.Parse(x))
        let rec parseInnerNodes (part: ParserState) (spec: (string * SchemaType) list) (nodes: XElement list) =
            match part, nodes with
            | _, [] -> spec
            | Begin, (Xsd "annotation" as node)::nodes ->
                parseInnerNodes Annotation spec nodes
            | (Begin | Annotation | Content), ((Xsd "group" | Xsd "choice" | Xsd "sequence" | Xsd "any") as node)::nodes ->
                failwithf "Content element %A for Sequence element is not implemented yet!" node.Name
            | (Begin | Annotation | Content), (Xsd "element" as node)::nodes ->
                parseInnerNodes Content (parseElement(node)::spec) nodes
            | _, node::_ ->
                failwithf "Element %s was not expected at the current position." node.Name.LocalName
        node.Elements() |> List.ofSeq |> parseInnerNodes Begin []

    and parseAttribute (node: XElement) =
        (*<attribute    default=string
                        fixed=string
                        form=qualified|unqualified
                        id=ID
                        name=NCName
                        ref=QName
                        type=QName
                        use=optional|prohibited|required
                        any attributes>
            annotation?,simpleType?
          </attribute>*)
        let rec parseInnerNodes (part: ParserState) (spec: TypeDefinition) (nodes: XElement list) =
            match part, nodes with
            | _, [] ->
                spec
            | Begin, (Xsd "annotation" as node)::nodes ->
                parseInnerNodes Annotation spec nodes
            | (Begin | Annotation), (Xsd "simpleType" as node)::nodes ->
                failwith "SimpleType element for Attribute element is not implemented yet!"
            | _, node::_ ->
                failwithf "Element %s was not expected at the current position." node.Name.LocalName
        let name = node |> reqAttr (XName.Get("name"))
        match node |> attr (XName.Get("type")) with
        | Some value -> (name, TypeReference(parseXName node value))
        | _ ->
            let typeDef = { Attributes = []; Properties = [] }
            (name, node.Elements() |> List.ofSeq |> parseInnerNodes Begin typeDef |> TypeDefinition)

    and parseAll (node: XElement) =
        let minOccurs = match node |> attrOrDefault (XName.Get("minOccurs")) "1" with
                        | "0" -> 0
                        | "1" -> 1
                        | x -> failwithf "Unexpected minOccurs value %s." x
        let maxOccurs = match node |> attrOrDefault (XName.Get("maxOccurs")) "1" with
                        | "1" -> 1
                        | x -> failwithf "Unexpected maxOccurs value %s." x
        let rec parseInnerNodes (part: ParserState) (spec: (string * SchemaType) list) (nodes: XElement list) =
            match part, nodes with
            | _, [] ->
                spec
            | Begin, (Xsd "annotation" as node)::nodes ->
                parseInnerNodes Annotation spec nodes
            | (Begin | Annotation | Content), (Xsd "element" as node)::nodes ->
                parseInnerNodes Content (parseElement(node)::spec) nodes
            | _, node::_ ->
                failwithf "Element %s was not expected at the current position." node.Name.LocalName
        node.Elements() |> List.ofSeq |> parseInnerNodes Begin []

    and parseComplexContent (node: XElement) =
        let rec parseInnerNodes (state: ParserState) spec (nodes: XElement list) =
            match state, nodes with
            | _, [] -> spec
            | Begin, (Xsd "annotation" as node)::nodes -> parseInnerNodes Annotation spec nodes
            | (Begin | Annotation), (Xsd "restriction" as node)::nodes ->
                parseComplexContentRestriction node
                failwithf "Element %A inside complexContent element was not expected at the current position!" node.Name
            | (Begin | Annotation), (Xsd "extension" as node)::nodes ->
                failwithf "Element %A inside complexContent element was not expected at the current position!" node.Name
            | _, node::_ ->
                failwithf "Element %A inside complexContent element was not expected at the current position!" node.Name
        node.Elements() |> List.ofSeq |> parseInnerNodes Begin ()

    and parseComplexContentRestriction (node: XElement) =
        // annotation?,(group|all|choice|sequence)?,(attribute|attributeGroup)*,anyAttribute?
        let rec parseInnerNodes (state: ParserState) spec (nodes: XElement list) =
            match state, nodes with
            | _, [] -> spec
            | Begin, (Xsd "annotation" as node)::nodes -> parseInnerNodes Annotation spec nodes
            | (Begin | Annotation), (Xsd "group" as node)::nodes
            | (Begin | Annotation), (Xsd "all" as node)::nodes
            | (Begin | Annotation), (Xsd "choice" as node)::nodes ->
                failwithf "Element %A inside complexContent restriction element is not implemented." node.Name
            | (Begin | Annotation), (Xsd "sequence" as node)::nodes ->
                failwithf "Element %A inside complexContent restriction element is not implemented." node.Name
            | (Begin | Annotation | Particle | Attribute), (Xsd "attribute" as node)::nodes
            | (Begin | Annotation | Particle | Attribute), (Xsd "attributeGroup" as node)::nodes ->
                failwithf "Element %A inside complexContent restriction element is not implemented." node.Name
            | (Begin | Annotation | Particle | Attribute), (Xsd "anyAttribute" as node)::nodes ->
                failwithf "Element %A inside complexContent restriction element is not implemented." node.Name
            | _, node::_ ->
                failwithf "Element %A inside complexContent restriction element was not expected at the current position!" node.Name
        node.Elements() |> List.ofSeq |> parseInnerNodes Begin ()

    let parseSchemaNode (node: XElement) =
        let snode = { QualifiedAttributes = node |> isQualified (XName.Get("attributeFormDefault"))
                      QualifiedElements = node |> isQualified (XName.Get("elementFormDefault"))
                      TargetNamespace = node |> attrOrDefault (XName.Get("targetNamespace")) "" |> XNamespace.Get
                      Includes = []
                      Imports = []
                      Elements = Dictionary<XName,SchemaType>()
                      Types = Dictionary<XName,TypeDefinition>() }
        node.Elements()
        |> Seq.fold (fun (state, snode) node ->
            match state, node with
            | _, Xsd "annotation" ->
                state, snode
            | (Begin | Header), Xsd "include" ->
                let schloc = node |> reqAttr (XName.Get("schemaLocation"))
                Header, { snode with Includes = Uri(schloc)::snode.Includes }
            | (Begin | Header), Xsd "import" ->
                let ns = node |> attrOrDefault (XName.Get("namespace")) ""
                let schloc = node |> attr (XName.Get("schemaLocation")) |> Option.map (fun x -> Uri(x))
                Header, { snode with Imports = (XNamespace.Get(ns), schloc)::snode.Imports }
            | (Begin | Header), Xsd "redefine" ->
                failwithf "Element %A inside schema element is not implemented yet." node.Name
            | _, Xsd "complexType" ->
                let name = node |> reqAttr (XName.Get("name"))
                snode.Types.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), parseComplexType node)
                TypeSpec, snode
            | _, Xsd "element" ->
                let name, tp = parseElement node
                snode.Elements.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), tp)
                TypeSpec, snode
            | _, (Xsd "simpleType" | Xsd "group" | Xsd "attributeGroup" | Xsd "attribute" | Xsd "notation") ->
                failwithf "Element %A inside schema element is not implemented yet." node.Name
            | _ ->
                failwithf "Element %A inside schema element was not expected at the current position!" node.Name
            ) (Begin, snode)
        |> snd

    let parseSchema (definitions: XElement) =
        match definitions.Element(XName.Get("types", XmlNamespace.Wsdl)) with
        | null -> []
        | typesNode ->
            typesNode.Elements(XName.Get("schema", XmlNamespace.Xsd))
            |> Seq.map parseSchemaNode
            |> List.ofSeq

type Schema =
  { TypeSchemas: XsdSchema.SchemaNode list
    Services: Service list }

let readSchema (uri: string) =
    let document = XDocument.Load(uri)
    let definitionsNode = document.Element(XName.Get("definitions", XmlNamespace.Wsdl))
    { Services = definitionsNode |> parseServices
      TypeSchemas = definitionsNode |> XsdSchema.parseSchema }

let (|IsXRoadHeader|) (part: MessagePart) =
    match part.Reference with
    | SchemaElement name -> match name with
                            | XsdSchema.XrdType "consumer"
                            | XsdSchema.XrdType "producer"
                            | XsdSchema.XrdType "userId"
                            | XsdSchema.XrdType "id"
                            | XsdSchema.XrdType "service"
                            | XsdSchema.XrdType "issue"
                            | XsdSchema.XrdType "unit"
                            | XsdSchema.XrdType "position"
                            | XsdSchema.XrdType "userName"
                            | XsdSchema.XrdType "async"
                            | XsdSchema.XrdType "authenticator"
                            | XsdSchema.XrdType "paid"
                            | XsdSchema.XrdType "encrypt"
                            | XsdSchema.XrdType "encryptCert"
                            | XsdSchema.XrdType "encrypted"
                            | XsdSchema.XrdType "encryptedCert" -> true
                            | _ -> false
    | _ -> false
