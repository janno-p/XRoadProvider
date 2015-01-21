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
    | [|name|] -> XName.Get(name)
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
    let soapOperation = operation.Element(XName.Get("operation", XmlNamespace.Soap))
    let bindingStyle = match soapOperation |> attr (XName.Get("style")) with
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

type FieldType = Attribute | Property

type Field =
  { Name: string
    TypeName: XName
    FieldType: FieldType }

type ContainmentType = All | Sequence

type SchemaTypeSpec =
  { Name: string
    Fields: FieldType list
    ContainmentType: ContainmentType }

type SchemaType =
    | Reference of XmlReference
    | Spec of SchemaTypeSpec

type SchemaTypeCollection =
  { Namespace: XNamespace
    SchemaTypes: IDictionary<XmlReference,SchemaType> }

type Schema =
  { TypeCollections: SchemaTypeCollection list
    Services: Service list }

let (|Xsd|_|) (element: XElement) =
    match element.Name.NamespaceName with
    | XmlNamespace.Xsd -> Some element.Name.LocalName
    | _ -> None

let parseXsdElement (node: XElement) =
    // annotation?,(simpleType|complexType)?,(unique|key|keyref)*
    let rec parseInternal (data: XElement option * XElement option * bool) (nodes: XElement list) =
        match nodes with
        | [] -> data
        | (Xsd "annotation" as n)::nodes ->
            match data with
            | None, None, false -> parseInternal (Some n, None, false) nodes
            | _ -> failwithf "Annotation element was not expected in %A." node.Name
        | ((Xsd "simpleType" | Xsd "complexType") as n)::nodes ->
            match data with
            | x, None, false -> parseInternal (x, Some n, false) nodes
            | _ -> failwithf "Type element was not expected in %A." node.Name
        | ((Xsd "unique" | Xsd "key" | Xsd "keyref") as n)::nodes ->
            match data with
            | x, y, _ -> parseInternal (x, y, true) nodes
        | n::nodes -> failwithf "Element %A was not expected in %A." n.Name node.Name
    let result = node.Elements() |> List.ofSeq |> parseInternal (None, None, false)
    match result with (x, y, _) -> (x, y)

let parseTypeSchema (schema: XElement) =
    // (include|import|redefine|annotation)*,((simpleType|complexType|group|attributeGroup|element|attribute|notation),annotation*)*
    let targetNamespace = XNamespace.Get(schema |> attrOrDefault (XName.Get("targetNamespace")) "")
    let typeCache = Dictionary<XmlReference,SchemaType>()
    let rec parseSchemaDef (element: XElement): XmlReference * SchemaType =
        match element with
        | Xsd "element" -> // Only top-level elements should match this pattern
            let annotation, elementType = parseXsdElement (element)
            let name = element |> reqAttr (XName.Get("name"))
            let key = SchemaElement(XName.Get(name, targetNamespace.NamespaceName))
            match element |> attr (XName.Get("type")) with
            | Some value ->
                let typeName = parseXName element value
                if typeName.NamespaceName <> targetNamespace.NamespaceName then
                    failwithf "Cannot parse type %A, because external namespaces are not supported yet!" typeName
                (key, Reference(SchemaType(typeName)))
            | _ ->
                match elementType with
                | Some node -> (key, snd (parseSchemaDef node))
                | _ -> failwithf "Top-level element %A has no type info." element.Name
        | Xsd "complexType" ->
            // annotation?,(simpleContent|complexContent|((group|all|choice|sequence)?,((attribute|attributeGroup)*,anyAttribute?)))
            //(_,_)
            failwith "TODO: Implement complexType!!"
        | Xsd "simpleType" ->
            // annotation?,(restriction|list|union)
            //(_,_)
            failwith "TODO: Implement simpleType!!"
        | Xsd "group" ->
            // annotation?,(restriction|list|union)
            //(_,_)
            failwith "TODO: Implement group!!"
        | Xsd "attribute" ->
            // annotation?,(restriction|list|union)
            //(_,_)
            failwith "TODO: Implement attribute!!"
        | Xsd "attributeGroup" ->
            // annotation?,(restriction|list|union)
            //(_,_)
            failwith "TODO: Implement attributeGroup!!"
        | _ -> failwith "never"
    schema.Elements() |> Seq.map parseSchemaDef |> Seq.iter typeCache.Add
    targetNamespace, typeCache

let parseTypeSchemas (definitions: XElement) =
    match definitions.Element(XName.Get("types", XmlNamespace.Wsdl)) with
    | null -> []
    | element ->
        element.Elements(XName.Get("schema", XmlNamespace.Xsd))
        |> Seq.map (fun e ->
            let tns, tc = parseTypeSchema e
            { Namespace = tns; SchemaTypes = tc })
        |> List.ofSeq

let readSchema (uri: string) =
    let document = XDocument.Load(uri)
    let definitionsNode = document.Element(XName.Get("definitions", XmlNamespace.Wsdl))
    { Services = definitionsNode |> parseServices
      TypeCollections = definitionsNode |> parseTypeSchemas }







module XsdSchema =
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
        Elements: IDictionary<XName,SchemaType> }

    let isQualified attrName node =
        match node |> attrOrDefault attrName "unqualified" with
        | "qualified" -> true
        | "unqualified" -> false
        | x -> failwithf "Unknown %s value '%s'" attrName.LocalName x

    type SchemaExpr =
        | Empty
        | Item of string
        | Maybe of SchemaExpr
        | More of SchemaExpr
        | Or of SchemaExpr list
        | Sequence of SchemaExpr list

    let eval (exp: SchemaExpr) nodeName =
        let rec evalInner (exp: SchemaExpr) =
            match exp with
            | Empty -> (false, Empty)
            | Item x -> if x = nodeName then (true, Empty) else (false, Empty)
            | Maybe x -> let (_, rem) = evalInner x
                         (true, rem)
            | More x -> match evalInner x with
                        | false, rem -> (true, rem)
                        (res)


    let walkExpr (expr: SchemaExpr) nodeName =
        match expr with
        | Empty -> failwithf "Unexpected element %s when none was allowed." nodeName
        | Item x -> if x = nodeName then Empty
                    else failwithf "Unexpected element %s when only %s was allowed." x nodeName
        | Maybe expr

    let complexTypeExpr =
        Sequence [(Maybe (Item "annotation"))
                  (Or [(Item "simpleContent")
                       (Item "complexContent")
                       (Sequence [(Maybe (Or [(Item "group"); (Item "all"); (Item "choice"); (Item "sequence")]))
                                  (More (Or [(Item "attribute"); (Item "attributeGroup")]))
                                  (Maybe (Item "anyAttribute"))])])]

    let parseComplexType (node: XElement) =
        let isAbstract = match node |> attrOrDefault (XName.Get("abstract")) "false" with
                         | "true" -> true
                         | "false" -> false
                         | x -> failwithf "Invalid value %s for complexType::abstract attribute" x
        let rec parseInnerNodes (exp: SchemaExpr) (nodes: XElement list) =
            match nodes with
            | [] -> typeSpec
            | ((Xsd "annotation") as node)::nodes ->
                let exp = walkExpr exp
                ()
            | ((Xsd "simpleContent" | Xsd "complexContent") as node)::nodes -> ()
            | ((Xsd "group" | Xsd "all" | Xsd "choice" | Xsd "sequence") as node)::nodes -> ()
            | ((Xsd "attribute" | Xsd "attributeGroup") as node)::nodes -> ()
            | ((Xsd "anyAttribute") as node)::nodes -> ()
        (*<complexType  id=ID
                        name=NCName
                        abstract=true|false
                        mixed=true|false
                        block=(#all|list of (extension|restriction))
                        final=(#all|list of (extension|restriction))
                        any attributes>
            annotation?,
            (
                simpleContent|complexContent|
                (
                    (group|all|choice|sequence)?,
                    (attribute|attributeGroup)*,
                    anyAttribute?
                )
            )
          </complexType>*)
        node.Elements() |> List.ofSeq |> parseInnerNodes complexTypeExpr

    let parseSchemaNode (node: XElement) =
        let qattr = node |> isQualified (XName.Get("attributeFormDefault"))
        let qelem = node |> isQualified (XName.Get("elementFormDefault"))
        let tns = node |> attrOrDefault (XName.Get("targetNamespace")) "" |> XNamespace.Get
        let snode = { QualifiedAttributes = qattr
                      QualifiedElements = qelem
                      TargetNamespace = tns
                      Includes = []
                      Imports = []
                      Elements = Dictionary<XName,SchemaType>() }
        let rec parseInnerNodes (snode: SchemaNode) cursor (nodes: XElement list) =
            // (include|import|redefine|annotation)*,((simpleType|complexType|group|attributeGroup|element|attribute|notation),annotation*)*
            match nodes with
            | [] -> snode
            | ((Xsd "annotation") as node)::nodes -> // Ignored
                parseInnerNodes snode cursor nodes
            | ((Xsd "include" | Xsd "import" | Xsd "redefine") as node)::nodes ->
                match cursor with
                | false ->
                    match node.Name.LocalName with
                    | "include" ->
                        let schloc = node |> reqAttr (XName.Get("schemaLocation"))
                        parseInnerNodes { snode with Includes = Uri(schloc)::snode.Includes } cursor nodes
                    | "import" ->
                        let ns = node |> attrOrDefault (XName.Get("namespace")) ""
                        let schloc = node |> attr (XName.Get("schemaLocation")) |> Option.map (fun x -> Uri(x))
                        parseInnerNodes { snode with Imports = (XNamespace.Get(ns), schloc)::snode.Imports } cursor nodes
                    | x -> failwithf "Subelement %s for schema element is not supported." x
                | _ -> failwithf "Unexpected %s element. Expected simpleType, complexType, group, attributeGroup, element, attribute, notation or annotation." node.Name.LocalName
            | ((Xsd "simpleType" | Xsd "complexType" | Xsd "group" | Xsd "attributeGroup" | Xsd "element" | Xsd "attribute" | Xsd "notation") as node)::nodes ->
                let name = node |> reqAttr (XName.Get("name"))
                match node.Name.LocalName with
                | "complexType" ->
                    // annotation?,(simpleContent|complexContent|((group|all|choice|sequence)?,((attribute|attributeGroup)*,anyAttribute?)))
                    parseInnerNodes snode true nodes
                | "element" ->
                    match node |> attr (XName.Get("type")) with
                    | Some value ->
                        snode.Elements.Add(XName.Get(name, tns.NamespaceName), TypeReference(parseXName node value))
                        parseInnerNodes snode true nodes
                    | _ ->
                        // annotation?,(simpleType|complexType)?,(unique|key|keyref)*
                        let typeDef = { Attributes = []; Properties = [] }
                        snode.Elements.Add(XName.Get(name, tns.NamespaceName), TypeDefinition(typeDef))
                        parseInnerNodes snode true nodes
                | x -> failwithf "Subelement %s for schema element is not supported." x
            | node::nodes -> failwithf "Unexpected subelement %A for schema element." node.Name
        node.Elements() |> List.ofSeq |> parseInnerNodes snode false
