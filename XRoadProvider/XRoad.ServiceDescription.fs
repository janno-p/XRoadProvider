module internal XRoad.ServiceDescription

open System.Collections.Generic
open System.Xml.Linq

open XRoad.Common

/// Globally unique identifier for Xml Schema elements and types.
type SchemaName =
    | SchemaElement of XName
    | SchemaType of XName
    member this.XName with get() = match this with SchemaElement(name) | SchemaType(name) -> name

/// Operation binding styles supported by X-Road infrastucture.
type OperationStyle =
    | RpcEncoded
    | DocLiteral
    /// Each binding style corresponds to certain X-Road namespace.
    member this.Namespace =
        match this with
        | RpcEncoded -> XmlNamespace.Xtee // Legacy
        | DocLiteral -> XmlNamespace.XRoad
    /// Style name used in WSDL document
    override this.ToString() =
        match this with
        | RpcEncoded -> "rpc"
        | DocLiteral -> "document"

/// WSDL document message part description.
type MessagePart =
    { Name: string
      SchemaEntity: SchemaName }

/// Describes main part of the operation.
type OperationBody =
    { Namespace: string
      Parts: MessagePart list }
    /// Initializes empty body.
    static member Empty = { Namespace = ""; Parts = [] }

/// Defines all allowed parts in operation request or response message.
type OperationMessage =
    { Name: XName
      Body: OperationBody
      Header: MessagePart list
      MultipartContent: MessagePart list }
    /// Initializes empty message.
    static member Create(name) =
        { Name = name
          Body = OperationBody.Empty
          Header = []
          MultipartContent = [] }

/// Holds description of entire message.
type Operation =
    { Name: XName
      Version: string option
      Style: OperationStyle
      Request: OperationMessage
      Response: OperationMessage
      Documentation: IDictionary<string,string> }
    /// Reads list of required X-Road header elements from operation definition.
    member this.GetRequiredHeaders () =
        let headers, rest =
            this.Request.Header
            |> List.partition (fun part ->
                match part.SchemaEntity with
                | SchemaElement(XteeHeader(_)) when this.Style = RpcEncoded -> true
                | SchemaElement(XRoadHeader(_)) when this.Style = DocLiteral -> true
                | _ -> false)
        if rest.Length > 0
        then failwithf "Unhandled SOAP Header elements detected: %A" rest
        else headers |> List.map (fun part -> part.Name)

/// Collects multiple operations into logical group.
type PortBinding =
    { Name: string
      Address: string
      Producer: string
      Documentation: IDictionary<string,string>
      Operations: Operation list
      Style: OperationStyle }

/// All operations defined for single producer.
type Service =
    { Name: string
      Ports: PortBinding list }

/// Separate module to contain functionality of parsing service description from WSDL document.
module Parser =
    /// Checks validity of the operation style according to X-Road specification.
    /// Mixing styles is not allowed, and operation style should match binding style.
    let private validateOperationStyle styleValue style =
        match styleValue, style with
        | "document", RpcEncoded -> failwith "Binding style `document` doesn't match legacy message format."
        | "rpc", DocLiteral -> failwith "Binding style `rpc` doesn't match new message format."
        | "document", DocLiteral
        | "rpc", RpcEncoded -> ()
        | x, _ -> failwithf "Unknown SOAP binding style %s" x

    /// Parse X-Road title elements for various languages.
    let private readLanguages (element: XElement) =
        element.Elements(xnsname "title" XmlNamespace.XRoad)
        |> Seq.fold (fun (doc: IDictionary<_,_>) el ->
            let lang = el |> attrOrDefault (xnsname "lang" XmlNamespace.Xml) "en"
            doc.[lang] <- el.Value
            doc) (upcast Dictionary<_,_>())

    /// Read documentation element contents into language code indexed dictionary.
    let private readDocumentation (element: XElement) =
        match element.Element(xnsname "documentation" XmlNamespace.Wsdl) with
        | null -> dict []
        | element -> readLanguages element

    /// Parse qualified name for message attribute value.
    let private parseMessageName name (element: XElement) =
        let messageElement = element.Element(xnsname name XmlNamespace.Wsdl)
        messageElement
        |> reqAttr (xname "message")
        |> parseXName messageElement

    /// Locate message element definition in WSDL document.
    /// http://www.w3.org/TR/wsdl#_messages
    let private findMessageElement definitions (name: XName) =
        // Default namespace for messages
        let targetNamespace = definitions |> attrOrDefault (xname "targetNamespace") ""
        if name.NamespaceName <> targetNamespace then
            failwithf "External messages are not supported yet! [%O]" name
        definitions.Elements(xnsname "message" XmlNamespace.Wsdl)
        |> Seq.find (fun el -> (el |> reqAttr (xname "name")) = name.LocalName)

    /// Collect parts of given message.
    /// Returns name-indexed map of schema entities.
    /// http://www.w3.org/TR/wsdl#_message
    let private parseAbstractParts msgName (abstractDef: XElement) =
        abstractDef.Elements(xnsname "part" XmlNamespace.Wsdl)
        |> Seq.map (fun elem ->
            let name = elem |> attrOrDefault (xname "name") ""
            match (elem |> attr (xname "element")), (elem |> attr (xname "type")) with
            | Some el, _ -> name, SchemaElement(parseXName elem el)
            | _, Some tp -> name, SchemaType(parseXName elem tp)
            | _ -> failwithf "Unknown element or type for message %s part %s" msgName name)
        |> Map.ofSeq

    /// Parse primary operation parameters (operation body).
    /// http://www.w3.org/TR/wsdl#_soap:body
    let private parseSoapBody msgName ns (style: OperationStyle) (abstractParts: Map<string,SchemaName>) (elem: XElement) (opmsg: OperationMessage) =
        // Get namespace for root element of the operation.
        let ns =
            match elem |> reqAttr(xname "use"), style with
            | "literal", RpcEncoded -> failwith "Invalid use value 'literal' for RPC style, only 'encoded' is allowed."
            | "literal", DocLiteral -> ns
            | "encoded", DocLiteral -> failwith "Invalid use value 'encoded' for RPC style, only 'literal' is allowed."
            | "encoded", RpcEncoded -> elem |> reqAttr(xname "namespace")
            | u, _ -> failwithf "Unexpected use/style combination: %A/%A" u style
        let opmsg =
            // If parts attribute is defined, use space-separated list to get specified parts.
            // When no explicit parts are given, then all remaining parts not used in other blocks are included into body.
            match elem |> attr (xname "parts") with
            | Some value ->
                value.Split(' ')
                |> Array.fold (fun om partName ->
                    match abstractParts.TryFind partName with
                    | Some(part) ->
                        { om with
                            Body =
                                if om.Body.Parts |> List.exists (fun x -> x.Name = partName) then om.Body
                                else { om.Body with Parts = { Name = partName; SchemaEntity = part } :: om.Body.Parts } }
                    | None -> failwithf "Message %s does not contain part %s" msgName partName
                    ) opmsg
            | _ -> opmsg
        // Apply specified namespace to operation root (body) element.
        { opmsg with Body = { opmsg.Body with Namespace = ns } }

    /// Parse header elements defined in concrete binding.
    /// http://www.w3.org/TR/wsdl#_soap:header
    let private parseSoapHeader (style: OperationStyle) (definitions: XElement) (elem: XElement) (opmsg: OperationMessage) =
        let messageName = elem |> reqAttr (xname "message") |> parseXName elem
        let partName = elem |> reqAttr (xname "part")
        match elem |> reqAttr (xname "use"), style with
        | "literal", RpcEncoded -> failwith "Invalid use value literal for RPC style, only encoded is allowed."
        | "encoded", DocLiteral -> failwith "Invalid use value encoded for document style, only literal is allowed."
        | _ -> ()
        let message = findMessageElement definitions messageName
        let parts = message |> parseAbstractParts messageName.LocalName
        match parts.TryFind partName with
        | Some(value) -> { opmsg with Header = { Name = partName; SchemaEntity = value } :: opmsg.Header }
        | None -> failwithf "Message %s does not contain part %s" messageName.LocalName partName

    /// Read operation message and its parts definitions from document.
    /// http://www.w3.org/TR/wsdl#_abstract-v
    let private parseOperationMessage (style: OperationStyle) (binding: XElement) definitions abstractDef ns =
        let msgName = abstractDef |> reqAttr (xname "name")
        let abstractParts = abstractDef |> parseAbstractParts msgName
        let parseSoapBody' = parseSoapBody msgName ns style abstractParts
        let parseSoapHeader' = parseSoapHeader style definitions
        // Walk through message parts explicitly referenced in operation binding.
        let operationMessage =
            binding.Elements()
            |> Seq.fold (fun opmsg elem ->
                match elem.Name.NamespaceName, elem.Name.LocalName with
                | XmlNamespace.Soap, "body" -> opmsg |> parseSoapBody' elem
                | XmlNamespace.Soap, "header" -> opmsg |> parseSoapHeader' elem
                | XmlNamespace.Mime, "multipartRelated" ->
                    elem.Elements(xnsname "part" XmlNamespace.Mime)
                    |> Seq.fold (fun opmsg elem ->
                        elem.Elements()
                        |> Seq.fold (fun opmsg elem ->
                            match elem.Name.NamespaceName, elem.Name.LocalName with
                            | XmlNamespace.Soap, "body" -> opmsg |> parseSoapBody' elem
                            | XmlNamespace.Soap, "header" -> opmsg |> parseSoapHeader' elem
                            | XmlNamespace.Mime, "content" ->
                                let partName = elem |> reqAttr (xname "part")
                                match abstractParts.TryFind partName with
                                | Some(value) -> { opmsg with MultipartContent = { Name = partName; SchemaEntity = value } :: opmsg.MultipartContent }
                                | None -> failwithf "Message %s does not contain part %s" msgName partName
                            | _ -> opmsg) opmsg) opmsg
                | _ -> opmsg) (OperationMessage.Create(xnsname msgName ns))
        // All remaining parts are included in body definition.
        abstractParts
        |> Seq.fold (fun opmsg p ->
            if opmsg.Body.Parts |> List.exists (fun x -> x.Name = p.Key) then opmsg
            elif opmsg.MultipartContent |> List.exists (fun x -> x.Name = p.Key) then opmsg
            else { opmsg with Body = { opmsg.Body with Parts = { Name = p.Key; SchemaEntity = p.Value } :: opmsg.Body.Parts } }
            ) operationMessage

    /// Parse operation binding and bind to abstract message definitions.
    /// http://www.w3.org/TR/wsdl#_bindings
    let private parseOperation operation (portType: XElement) (definitions: XElement) (style: OperationStyle) ns =
        let name = operation |> reqAttr (xname "name")
        // Extract X-Road version of the operation (optional: not used for metaservice operations).
        let version =
            match operation.Element(xnsname "version" style.Namespace) with
            | null -> None
            | el -> Some el.Value
        // SOAP extension for operation element: http://www.w3.org/TR/wsdl#_soap:operation
        match operation.Element(xnsname "operation" XmlNamespace.Soap) with
        | null -> ()
        | soapOperation ->
            let styleValue = soapOperation |> attrOrDefault (xname "style") (style.ToString())
            validateOperationStyle styleValue style
        // Find abstract definition for the operation in matching portType element.
        let abstractDesc =
            portType.Elements(xnsname "operation" XmlNamespace.Wsdl)
            |> Seq.find (fun op -> (op |> reqAttr (xname "name")) = name)
        let inputMessage = abstractDesc |> parseMessageName "input" |> findMessageElement definitions
        let outputMessage = abstractDesc |> parseMessageName "output" |> findMessageElement definitions
        // Combine abstract and concrete part information about service implementation.
        { Name = xnsname name ns
          Version = version
          Style = style
          Request = parseOperationMessage style (operation.Element(xnsname "input" XmlNamespace.Wsdl)) definitions inputMessage ns
          Response = parseOperationMessage style (operation.Element(xnsname "output" XmlNamespace.Wsdl)) definitions outputMessage ns
          Documentation = readDocumentation abstractDesc }

    /// Parse operations bindings block.
    /// http://www.w3.org/TR/wsdl#_bindings
    let private parseBinding (definitions: XElement) (bindingName: XName) (portBinding: PortBinding) =
        // Default namespace for operations
        let targetNamespace = definitions |> attrOrDefault (xname "targetNamespace") ""
        // Find binding element in current document
        if bindingName.NamespaceName <> targetNamespace then
            failwithf "External namespaces are not yet supported! Given %s." bindingName.NamespaceName
        let binding =
            definitions.Elements(xnsname "binding" XmlNamespace.Wsdl)
            |> Seq.find (fun el -> (el |> reqAttr (xname "name")) = bindingName.LocalName)
        // Find portType element in current document for abstract part definitions.
        let portTypeName = binding |> reqAttr (xname "type") |> parseXName binding
        if portTypeName.NamespaceName <> targetNamespace then
            failwithf "External namespaces are not yet supported! Given %s." portTypeName.NamespaceName
        let portType =
            definitions.Elements(xnsname "portType" XmlNamespace.Wsdl)
            |> Seq.find (fun el -> (el |> reqAttr (xname "name")) = portTypeName.LocalName)
        // SOAP extension for binding element: http://www.w3.org/TR/wsdl#_soap:binding
        let soapBinding = binding.Element(xnsname "binding" XmlNamespace.Soap)
        validateOperationStyle (soapBinding |> attrOrDefault (xname "style") "document") portBinding.Style
        // X-Road specification allows only HTTP transport.
        let transport = soapBinding |> attrOrDefault (xname "transport") ""
        if transport <> XmlNamespace.Http then
            failwithf "Only HTTP transport is allowed. Specified %s" transport
        // Parse individual operations from current binding element.
        { portBinding with
            Operations =
                binding.Elements(xnsname "operation" XmlNamespace.Wsdl)
                |> Seq.map (fun op -> parseOperation op portType definitions portBinding.Style portTypeName.NamespaceName)
                |> List.ofSeq }

    /// Parse port binding element contents.
    /// http://www.w3.org/TR/wsdl#_ports
    let private parsePortBinding definitions element =
        let name = element |> reqAttr (xname "name")
        let binding = element |> reqAttr (xname "binding") |> parseXName element
        // http://www.w3.org/TR/wsdl#_soap:address
        let address =
            match element.Element(xnsname "address" XmlNamespace.Soap) with
            | null -> ""
            | e -> e |> reqAttr (xname "location")
        // Extract producer name for given port from X-Road extension.
        let producer =
            let current = element.Element (xnsname "address" XmlNamespace.XRoad)
            let legacy = element.Element (xnsname "address" XmlNamespace.Xtee)
            match current, legacy with
            | null, null -> None
            | e, null | null, e ->
                let name = e |> attrOrDefault (xname "producer") ""
                let style = if e.Name.NamespaceName = XmlNamespace.XRoad then DocLiteral else RpcEncoded
                Some(name, style)
            | _ -> failwith "Mixing legacy operations with new message format is not supported."
        // Build port binding object if available.
        producer
        |> Option.map (fun (producer, style) ->
            { Name = name
              Address = address
              Producer = producer
              Documentation = readLanguages element
              Operations = []
              Style = style })
        |> Option.map (parseBinding definitions binding)

    /// Parse all service elements defined as immediate child elements of current element.
    /// http://www.w3.org/TR/wsdl#_services
    let parseServices (definitions: XElement) =
        definitions.Elements(xnsname "service" XmlNamespace.Wsdl)
        |> Seq.map (fun service ->
            { Name = service |> reqAttr (xname "name")
              Ports =
                service.Elements(xnsname "port" XmlNamespace.Wsdl)
                |> Seq.choose (parsePortBinding definitions)
                |> List.ofSeq })
        |> List.ofSeq
