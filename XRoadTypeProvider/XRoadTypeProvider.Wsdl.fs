module XRoadTypeProvider.Wsdl

open System
open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Linq

module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnvelope = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
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

let resolveType (qn: XmlQualifiedName) =
    match qn.Namespace with
    | XmlNamespace.XRoad -> mapXrdType qn.Name
    | _ -> failwithf "Unmapped type name %O" qn

let resolveElementType (qn: XmlQualifiedName) tns =
    match qn.Namespace with
    | XmlNamespace.XRoad -> mapXrdElementType qn.Name
    | XmlNamespace.Xtee -> mapXteeElementType qn.Name
    | ns when ns = tns -> typeof<obj>
    | _ -> failwithf "Unmapped element name %O" qn

let resolveUri uri =
    match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
    | true -> uri
    | _ ->
        let fullPath = (new FileInfo(uri)).FullName
        match File.Exists(fullPath) with
        | true -> fullPath
        | _ -> failwith (sprintf "Cannot resolve url location `%s`" uri)

type Operation =
  { Name: string
    Version: string option
    Style: XRoad.XRoadBindingStyle }

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

let parseOperation (operation: XElement) =
    let name = operation |> reqAttr (XName.Get("name"))
    let version = match operation.Element(XName.Get("version", XmlNamespace.XRoad)) with
                  | null -> None
                  | el -> Some el.Value
    let soapOperation = operation.Element(XName.Get("operation", XmlNamespace.Soap))
    let bindingStyle = match soapOperation |> attrOrDefault (XName.Get("style")) "document" with
                       | "document" -> XRoad.XRoadBindingStyle.DocumentLiteral
                       | "rpc" -> XRoad.XRoadBindingStyle.RpcEncoded
                       | x -> failwithf "Unknown SOAP binding style %s" x
    { Name = name; Version = version; Style = bindingStyle }

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
        |> Seq.map (fun op -> parseOperation op)
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
                let doc = servicePort.Elements(XName.Get("title", XmlNamespace.XRoad))
                          |> Seq.fold (fun (doc: IDictionary<string,string>) el ->
                              let lang = match el |> attr (XName.Get("lang", XmlNamespace.Xml)) with
                                         | Some lang -> lang
                                         | _ -> "en"
                              doc.[lang] <- el.Value
                              doc) portBinding.Documentation
                { portBinding with Name = name
                                   Address = address
                                   Producer = producer }
                |> parseBinding definitions binding)
        { Name = name; Ports = ports |> List.ofSeq })
    |> List.ofSeq

let readServices (uri: string) =
    use reader = XmlReader.Create(uri)
    let document = XDocument.Load(reader)
    let definitionsNode = document.Element(XName.Get("definitions", XmlNamespace.Wsdl))
    parseServices definitionsNode
