module internal XRoad.ServiceDescription

open System.Collections.Generic
open System.Xml.Linq

open XRoad.Common

/// Temporary type for SOAP:body binding elements.
type private SoapBody =
    { Parts: string list
      EncodingStyle: string option
      Namespace: string option }

/// Temporary type for SOAP:header binding elements.
type private SoapHeader =
    { Message: XName
      Part: string
      EncodingStyle: string option
      Namespace: string option }

/// Temporary type for MIME:content binding elements.
type private MimeContent =
    { Part: string
      Type: string option }

/// Parse X-Road title elements for various languages.
let private readLanguages languageCode (element: XElement) =
    element.Elements(xnsname "title" XmlNamespace.XRoad)
    |> Seq.fold (fun doc el ->
        let lang = el |> attrOrDefault (xnsname "lang" XmlNamespace.Xml) languageCode
        (lang, el.Value)::doc
        ) []
    |> List.tryFind (fst >> ((=) languageCode))
    |> Option.map snd

/// Read documentation element contents into language code indexed dictionary.
let private readDocumentation languageCode (element: XElement) =
    match element.Element(xnsname "documentation" XmlNamespace.Wsdl) with
    | null -> None
    | element -> readLanguages languageCode element

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

// Get encoding style used if any, and namespace for root element in RPC binding style.
let private getEncodingAndNamespace element =
    match element |> reqAttr (xname "use") with
    | "literal" -> None, None
    | "encoded" -> Some(element |> reqAttr (xname "encodingStyle")), element |> attr (xname "namespace")
    | encoding -> failwithf "Unexpected use attribute value `%s`" encoding

/// Parse primary operation parameters (operation body).
/// http://www.w3.org/TR/wsdl#_soap:body
let private parseSoapBody element =
    // Get encoding style used if any, and namespace for root element in RPC binding style.
    let encodingStyle, ns = getEncodingAndNamespace element
    // If parts attribute is defined, use space-separated list to get specified parts.
    // When no explicit parts are given, then all remaining parts not used in other blocks
    // are included into body.
    let parts =
        element
        |> attr (xname "parts")
        |> Option.map (fun value -> value.Split(' ') |> List.ofArray)
        |> Option.orDefault []
    // Apply specified namespace to operation root (body) element.
    { Parts = parts; EncodingStyle = encodingStyle; Namespace = ns }

/// Parse header elements defined in concrete binding.
/// http://www.w3.org/TR/wsdl#_soap:header
let private parseSoapHeader element =
    // Get encoding style used if any, and namespace for root element in RPC binding style.
    let encodingStyle, ns = getEncodingAndNamespace element
    let messageName = element |> reqAttr (xname "message") |> parseXName element
    let partName = element |> reqAttr (xname "part")
    { Message = messageName; Part = partName; EncodingStyle = encodingStyle; Namespace = ns }

/// Get message parts from service operation binding.
let private parseBindingParts (binding: XElement) _ (*messageName*) =
    // Validates SOAP:body contents.
    let foldBody oldBody elem isMultipart =
        oldBody
        |> Option.fold (fun newBody oldBody ->
            //if newBody <> oldBody
            //then failwithf "Multipart and non-multipart SOAP:body bindings do not match for message `%s`." messageName
            //else oldBody
            if isMultipart then newBody else oldBody) (parseSoapBody elem)
    // Parse binding elements.
    binding.Elements()
    |> Seq.fold (fun (bd: SoapBody option,hd,mp) elem ->
        match elem.Name.NamespaceName, elem.Name.LocalName with
        | XmlNamespace.Soap, "body" ->
            (Some(foldBody bd elem false),hd,mp)
        | XmlNamespace.Soap, "header" ->
            (bd, parseSoapHeader elem :: hd, mp)
        | XmlNamespace.Mime, "multipartRelated" ->
            elem %* xnsname "part" XmlNamespace.Mime
            |> Seq.collect (fun elem -> elem.Elements())
            |> Seq.fold (fun (bd,hd,mp) elem ->
                match elem.Name.NamespaceName, elem.Name.LocalName with
                | XmlNamespace.Soap, "body" ->
                    (Some(foldBody bd elem true), hd, mp)
                | XmlNamespace.Soap, "header" ->
                    (bd, parseSoapHeader elem :: hd, mp)
                | XmlNamespace.Mime, "content" ->
                    let partName = elem |> reqAttr (xname "part")
                    let contentType = elem |> attr (xname "type")
                    (bd,hd, { Part = partName; Type = contentType } :: mp)
                | _ -> (bd,hd,mp)) (bd,hd,mp)
        | _ -> (bd,hd,mp)
        ) (None, [], [])

/// Partition all message parts into body and header components.
let private partitionMessageParts (abstractParts: Map<_,_>)  bodyPart contentParts headerParts messageName definitions abstractDef (protocol: XRoadProtocol) =
    let contentParts =
        contentParts
        |> List.map (fun part ->
            match abstractParts.TryFind part.Part with
            | Some(_) -> part.Part
            | None -> failwithf "Message `%s` does not contain part `%s`." messageName part.Part)
    let parts =
        headerParts
        |> List.map (fun part ->
            let message = findMessageElement definitions part.Message
            if message = abstractDef then Choice1Of3(part.Part)
            else
                let parts = message |> parseAbstractParts part.Message.LocalName
                match parts.TryFind part.Part with
                | Some(value) when value.XName.NamespaceName = protocol.Namespace -> Choice2Of3(part.Part)
                | Some(_) -> Choice3Of3(part.Part)
                | None -> failwithf "Message %s does not contain part %s" part.Message.LocalName part.Part)
    let hdr = parts |> List.choose (fun x -> match x with Choice1Of3(x) -> Some(x) | _ -> None)
    let reqHdr = parts |> List.choose (fun x -> match x with Choice2Of3(x) -> Some(x) | _ -> None)
    let excludedParts = List.concat [ contentParts; hdr ]
    let body = abstractParts |> Map.toList |> List.map (fst) |> List.filter (fun x -> not (excludedParts |> List.exists ((=) x)))
    if not (List.isEmpty bodyPart.Parts) then
        let count = bodyPart.Parts |> List.filter (fun x -> body |> List.exists((=) x)) |> List.length
        if count <> body.Length then failwithf "Not all message `%s` parts have corresponding bindings." messageName
    body, reqHdr

/// Check if literal part of message is correct.
let private validateLiteralParameters (parameters: Parameter list) messageName =
    let typeCount =
        parameters
        |> List.filter (fun p -> match p.Type with SchemaType(_) -> true | _ -> false)
        |> List.length
    if typeCount > 1 || (typeCount = 1 && parameters.Length > 1)
    then failwithf "Literal operation message `%s` should have at most exactly one type reference in part definitions." messageName

/// Check if encoded part of message is correct.
let private validateEncodedParameters (parameters: Parameter list) messageName =
    if parameters |> List.filter (fun p -> match p.Type with SchemaElement(_) -> true | _ -> false) |> List.length > 0
    then failwithf "Encoded operation message `%s` should not have element references in part definitions." messageName

/// Read operation message and its parts definitions from document.
/// http://www.w3.org/TR/wsdl#_abstract-v
let private parseOperationMessage style (protocol: XRoadProtocol) (binding: XElement) definitions abstractDef ns =
    let msgName = abstractDef |> reqAttr (xname "name")
    let abstractParts = abstractDef |> parseAbstractParts msgName
    // Walk through message parts explicitly referenced in operation binding.
    let bodyPart, headerParts, contentParts = parseBindingParts binding msgName
    // Take wrapper name from body definition.
    let accessorName =
        match bodyPart with
        | Some(part) ->
            part.EncodingStyle
            |> Option.map (fun enc ->
                match enc with
                | XmlNamespace.SoapEnc -> xnsname msgName (part.Namespace |> Option.orDefault ns)
                | _ -> failwithf "Unknown encoding style `%s` for `%s` operation SOAP:body." enc msgName)
        | None -> failwithf "X-Road operation binding `%s` doesn't define SOAP:body." msgName
    // Build service parameters.
    let expectedBodyParts, requiredHeaders =
        partitionMessageParts abstractParts bodyPart.Value contentParts headerParts msgName definitions abstractDef protocol
    let parameters =
        expectedBodyParts
        |> List.map (fun partName ->
            match abstractParts.TryFind partName with
            | Some(typeName) -> { Name = partName; Type = typeName }
            | None -> failwithf "Message `%s` does not contain part `%s`." msgName partName)
    // Service request input or output parameters.
    let parameterWrapper =
        { HasMultipartContent = contentParts |> List.isEmpty |> not
          Parameters = parameters
          RequiredHeaders = requiredHeaders }
    // Validate parameter usage
    match accessorName with
    | Some(_) -> validateEncodedParameters parameterWrapper.Parameters msgName
    | None -> validateLiteralParameters parameterWrapper.Parameters msgName
    // Wrap method call into correct context.
    match style, accessorName with
    | Document, Some(value) -> DocEncodedCall(value.Namespace, parameterWrapper)
    | Document, None -> DocLiteralCall parameterWrapper
    | Rpc, Some(value) -> RpcEncodedCall(value, parameterWrapper)
    | Rpc, None -> RpcLiteralCall(xnsname msgName ns, parameterWrapper)

/// Parse operation binding and bind to abstract message definitions.
/// http://www.w3.org/TR/wsdl#_bindings
let private parseOperation languageCode operation portType definitions style ns (protocol: XRoadProtocol) =
    let name = operation |> reqAttr (xname "name")
    // Extract X-Road version of the operation (optional: not used for metaservice operations).
    let version =
        match operation %! xnsname "version" protocol.Namespace with
        | null -> None
        | el -> Some el.Value
    // SOAP extension for operation element: http://www.w3.org/TR/wsdl#_soap:operation
    let style =
        match operation %! xnsname "operation" XmlNamespace.Soap with
        | null -> style
        | soapOperation -> BindingStyle.FromNode(soapOperation, style)
    // Find abstract definition for the operation in matching portType element.
    let abstractDesc =
        let abstractOperation =
            portType %* xnsname "operation" XmlNamespace.Wsdl
            |> Seq.tryFind (fun op -> (op |> reqAttr (xname "name")) = name)
        match abstractOperation with
        | Some(op) -> op
        | None -> failwithf "Unable to find abstract definition for operation `%s` binding." name
    // Parse parameters for message input or output parameters.
    let parseParameters direction =
        let message = abstractDesc |> parseMessageName direction |> findMessageElement definitions
        let bindingElement = (operation %! xnsname direction XmlNamespace.Wsdl)
        parseOperationMessage style protocol bindingElement definitions message ns
    // Combine abstract and concrete part information about service implementation.
    { Name = name
      Version = version
      InputParameters = parseParameters "input"
      OutputParameters = parseParameters "output"
      Documentation = readDocumentation languageCode abstractDesc }

/// Parse operations bindings block.
/// http://www.w3.org/TR/wsdl#_bindings
let private parseBinding languageCode definitions (bindingName: XName) servicePort =
    // Default namespace for operations
    let targetNamespace = definitions |> attrOrDefault (xname "targetNamespace") ""
    // Find binding element in current document
    if bindingName.NamespaceName <> targetNamespace then
        failwithf "External namespaces are not yet supported! Given %s." bindingName.NamespaceName
    let binding =
        definitions %* xnsname "binding" XmlNamespace.Wsdl
        |> Seq.find (fun el -> (el |> reqAttr (xname "name")) = bindingName.LocalName)
    // Find portType element in current document for abstract part definitions.
    let portTypeName = binding |> reqAttr (xname "type") |> parseXName binding
    let ns = portTypeName.NamespaceName
    if ns <> targetNamespace then
        failwithf "External namespaces are not yet supported! Given %s." portTypeName.NamespaceName
    let portType =
        definitions %* xnsname "portType" XmlNamespace.Wsdl
        |> Seq.find (fun el -> (el |> reqAttr (xname "name")) = portTypeName.LocalName)
    // SOAP extension for binding element: http://www.w3.org/TR/wsdl#_soap:binding
    let soapBinding = binding %! xnsname "binding" XmlNamespace.Soap
    let bindingStyle = BindingStyle.FromNode(soapBinding)
    // X-Road specification allows only HTTP transport.
    let transport = soapBinding |> attrOrDefault (xname "transport") ""
    if transport <> XmlNamespace.Http then
        failwithf "Only HTTP transport is allowed. Specified %s" transport
    // Parse individual operations from current binding element.
    let methods =
        binding %* xnsname "operation" XmlNamespace.Wsdl
        |> Seq.map (fun op -> parseOperation languageCode op portType definitions bindingStyle ns servicePort.Protocol)
        |> List.ofSeq
    { servicePort with Methods = methods }

/// Parse port binding element contents.
/// http://www.w3.org/TR/wsdl#_ports
let private parsePortBinding languageCode definitions element =
    let name = element |> reqAttr (xname "name")
    let binding = element |> reqAttr (xname "binding") |> parseXName element
    // http://www.w3.org/TR/wsdl#_soap:address
    let address =
        match element %! xnsname "address" XmlNamespace.Soap with
        | null -> ""
        | e -> e |> reqAttr (xname "location")
    // Extract producer name for given port from X-Road extension.
    let current = element %! xnsname "address" XmlNamespace.XRoad
    let legacy = element %! xnsname "address" XmlNamespace.Xtee
    // Build port binding object if available.
    match current, legacy with
    | null, null -> None
    | e, null | null, e ->
        let producer = e |> reqAttr (xname "producer")
        let protocol = XRoadProtocol.FromNamespace(e.Name.NamespaceName)
        let servicePort =
            { Name = name
              Documentation = readLanguages languageCode element
              Uri = address
              Producer = producer
              Methods = []
              Protocol = protocol }
        Some(servicePort |> parseBinding languageCode definitions binding)
    | _ -> failwith "Mixing different X-Road protocol versions is not supported."

/// Parse all service elements defined as immediate child elements of current element.
/// http://www.w3.org/TR/wsdl#_services
let parseServices languageCode definitions =
    definitions %* xnsname "service" XmlNamespace.Wsdl
    |> Seq.map (fun service ->
        let ports =
            service %* xnsname "port" XmlNamespace.Wsdl
            |> Seq.choose (parsePortBinding languageCode definitions)
            |> List.ofSeq
        { Name = service |> reqAttr (xname "name"); Ports = ports})
    |> List.ofSeq
