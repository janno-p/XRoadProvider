namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.Serialization.Attributes

#if NET40
module internal Array =
    open System.Linq

    let exactlyOne (arr: 'T []) = arr.SingleOrDefault()
    let skip n (arr: 'T []) = arr.Skip(n).ToArray()

module internal List =
    let tryHead = function [] -> None | x::_ -> Some(x)
#endif

module internal MyOption =
    let defaultValue d o = o |> Option.fold (fun _ x -> x) d
    let defaultWith f o = match o with Some(o) -> o | None -> f()

type XRoadMessageProtocolVersion =
    | Version20 of string
    | Version30 of string
    | Version31Ee of string
    | Version31Eu of string
    | Version40
    with
        member this.ProducerName =
            match this with
            | Version20(s) | Version30(s) | Version31Ee(s) | Version31Eu(s) -> Some(s)
            | Version40 -> None
        member this.EnumValue =
            match this with
            | Version20(_) -> XRoadProtocol.Version20
            | Version30(_) -> XRoadProtocol.Version30
            | Version31Ee(_) -> XRoadProtocol.Version31Ee
            | Version31Eu(_) -> XRoadProtocol.Version31Eu
            | Version40(_) -> XRoadProtocol.Version40
        member this.HeaderNamespace =
            match this with
            | Version20(_) -> XmlNamespace.XRoad20
            | Version30(_) -> XmlNamespace.XRoad30
            | Version31Ee(_) -> XmlNamespace.XRoad31Ee
            | Version31Eu(_) -> XmlNamespace.XRoad31Eu
            | Version40(_) -> XmlNamespace.XRoad40

[<AutoOpen>]
module private XRoadProtocolExtensions =
    let protocolPrefix = function
        | XRoadProtocol.Version20 -> "xtee"
        | XRoadProtocol.Version30
        | XRoadProtocol.Version31Ee
        | XRoadProtocol.Version31Eu
        | XRoadProtocol.Version40 -> "xrd"
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

    let protocolNamespace = function
        | XRoadProtocol.Version20 -> XmlNamespace.XRoad20
        | XRoadProtocol.Version30 -> XmlNamespace.XRoad30
        | XRoadProtocol.Version31Ee -> XmlNamespace.XRoad31Ee
        | XRoadProtocol.Version31Eu -> XmlNamespace.XRoad31Eu
        | XRoadProtocol.Version40 -> XmlNamespace.XRoad40
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

    let private messageProtocolNamespace = function
        | Version20(_) -> XmlNamespace.XRoad20
        | Version30(_) -> XmlNamespace.XRoad30
        | Version31Ee(_) -> XmlNamespace.XRoad31Ee
        | Version31Eu(_) -> XmlNamespace.XRoad31Eu
        | Version40 -> XmlNamespace.XRoad40

    let private messageProtocolElementName name mpv = XName.Get(name, messageProtocolNamespace mpv)

    let titleElementName = messageProtocolElementName "title"
    let versionElementName = messageProtocolElementName "version"

    let private rpcHeaders = ["asutus"; "andmekogu"; "isikukood"; "ametnik"; "id"; "nimi"; "toimik"; "allasutus"; "amet"; "ametniknimi"; "asynkroonne"; "autentija"; "makstud"; "salastada"; "salastada_sertifikaadiga"; "salastatud"; "salastatud_sertifikaadiga"]
    let private docLegacyHeaders = ["consumer"; "producer"; "userId"; "id"; "service"; "issue"; "unit"; "position"; "userName"; "async"; "authenticator"; "paid"; "encrypt"; "encryptCert"; "encrypted"; "encryptedCert"]
    let private docHeaders = ["client"; "service"; "centralService"; "id"; "userId"; "requestHash"; "issue"; "protocolVersion"]

    let private isHeaderOf ns hdrs (xn: XName) = if xn.NamespaceName = ns then hdrs |> List.exists ((=) xn.LocalName) else false

    let isMessageProtocolHeaderFunc = function
        | Version20(_) -> isHeaderOf XmlNamespace.XRoad20 rpcHeaders
        | Version30(_) -> isHeaderOf XmlNamespace.XRoad30 docLegacyHeaders
        | Version31Ee(_) -> isHeaderOf XmlNamespace.XRoad31Ee docLegacyHeaders
        | Version31Eu(_) -> isHeaderOf XmlNamespace.XRoad31Eu docLegacyHeaders
        | Version40(_) -> isHeaderOf XmlNamespace.XRoad40 docHeaders

module internal XRoadTypeHelper =
    let getSystemTypeName = function
        | "NodaTime.LocalDate" -> Some(XmlQualifiedName("date", XmlNamespace.Xsd))
        | "NodaTime.LocalDateTime" -> Some(XmlQualifiedName("dateTime", XmlNamespace.Xsd))
        | "System.String" -> Some(XmlQualifiedName("string", XmlNamespace.Xsd))
        | "System.Boolean" -> Some(XmlQualifiedName("boolean", XmlNamespace.Xsd))
        | "System.Decimal" -> Some(XmlQualifiedName("decimal", XmlNamespace.Xsd))
        | "System.Double" -> Some(XmlQualifiedName("double", XmlNamespace.Xsd))
        | "System.Float" -> Some(XmlQualifiedName("float", XmlNamespace.Xsd))
        | "System.Int32" -> Some(XmlQualifiedName("int", XmlNamespace.Xsd))
        | "System.Numerics.BigInteger" -> Some(XmlQualifiedName("integer", XmlNamespace.Xsd))
        | "System.Int64" -> Some(XmlQualifiedName("long", XmlNamespace.Xsd))
        | _ -> None

[<AllowNullLiteral>]
type internal SerializerContext() =
    let attachments = Dictionary<string, BinaryContent>()
    member val IsMtomMessage = false with get, set
    member val IsMultipart = false with get, set
    member val Attachments = attachments with get
    member this.AddAttachment(contentID, content, useXop) =
        if useXop then this.IsMtomMessage <- true
        attachments.Add(contentID, content)
    member __.GetAttachment(href: string) =
        if href.StartsWith("cid:") then
            let contentID = href.Substring(4)
            match attachments.TryGetValue(contentID) with
            | true, value -> value
            | _ -> null;
        else failwithf "Invalid multipart content reference: `%s`." href

type internal MethodPartMap =
    { IsEncoded: bool
      IsMultipart: bool
      Accessor: XmlQualifiedName option }

type internal DeserializerDelegate = delegate of XmlReader * SerializerContext -> obj
type internal SerializerDelegate = delegate of XmlWriter * obj * SerializerContext -> unit
type internal OperationSerializerDelegate = delegate of XmlWriter * obj[] * SerializerContext -> unit

type internal MethodMap =
    { Deserializer: DeserializerDelegate
      Serializer: OperationSerializerDelegate
      Protocol: XRoadProtocol
      Request: MethodPartMap
      Response: MethodPartMap
      ServiceCode: string
      ServiceVersion: string option
      Namespaces: string list
      RequiredHeaders: IDictionary<string, string[]> }

module internal Wsdl =
    let (%!) (e: XElement) (xn: XName) = e.Element(xn)
    let (%*) (e: XElement) (xn: XName) = e.Elements(xn)

    /// Active patterns for matching XML document nodes from various namespaces.
    [<AutoOpen>]
    module Pattern =
        open NodaTime

        /// Matches names defined in `http://www.w3.org/2001/XMLSchema` namespace.
        let (|XsdName|_|) (name: XName) =
            match name.NamespaceName with
            | XmlNamespace.Xsd -> Some name.LocalName
            | _ -> None

        /// Matches names defined in `http://www.w3.org/XML/1998/namespace` namespace.
        let (|XmlName|_|) (name: XName) =
            match name.NamespaceName with
            | "" | null | XmlNamespace.Xml -> Some name.LocalName
            | _ -> None

        /// Matches names defined in `http://x-road.ee/xsd/x-road.xsd` namespace.
        let (|XrdName|_|) (name: XName) =
            match name.NamespaceName with
            | XmlNamespace.XRoad31Ee -> Some name.LocalName
            | _ -> None

        /// Matches names defined in `http://x-tee.riik.ee/xsd/xtee.xsd` namespace.
        let (|XteeName|_|) (name: XName) =
            match name.NamespaceName with
            | XmlNamespace.XRoad20 -> Some name.LocalName
            | _ -> None

        /// Matches names defined in `http://schemas.xmlsoap.org/soap/encoding/` namespace.
        let (|SoapEncName|_|) (name: XName) =
            match name.NamespaceName with
            | XmlNamespace.SoapEnc -> Some name.LocalName
            | _ -> None

        /// Matches elements defined in `http://www.w3.org/2001/XMLSchema` namespace.
        let (|Xsd|_|) (element: XElement) =
            match element.Name with
            | XsdName name -> Some name
            | _ -> None

        /// Matches type names which are mapped to system types.
        let (|SystemType|_|) = function
            | XsdName "anyURI" -> Some typeof<string>
            | XsdName "boolean" -> Some typeof<bool>
            | XsdName "date" -> Some typeof<LocalDate>
            | XsdName "dateTime" -> Some typeof<LocalDateTime>
            | XsdName "decimal" -> Some typeof<decimal>
            | XsdName "double" -> Some typeof<double>
            | XsdName "float" -> Some typeof<single>
            | XsdName "int" -> Some typeof<int>
            | XsdName "integer" -> Some typeof<bigint>
            | XsdName "long" -> Some typeof<int64>
            | XsdName "string" -> Some typeof<string>
            | XsdName "ID" -> Some typeof<string>
            | XsdName "NMTOKEN" -> Some typeof<string>
            | XsdName name -> failwithf "Unmapped XSD type %s" name
            | SoapEncName name -> failwithf "Unmapped SOAP-ENC type %s" name
            | _ -> None

        /// Matches system types which can be serialized as MIME multipart attachments:
        /// From X-Road service protocol: if the message is encoded as MIME container then values of all scalar elements
        /// of the input with type of either `xsd:base64Binary` or `xsd:hexBinary` will be sent as attachments.
        let (|BinaryType|_|) = function
            | XsdName "hexBinary"
            | XsdName "base64Binary"
            | SoapEncName "base64Binary" -> Some typeof<byte[]>
            | _ -> None

        /// Matches X-Road legacy format header elements.
        let (|XteeHeader|_|) name =
            match name with
            | XteeName "asutus"
            | XteeName "andmekogu"
            | XteeName "isikukood"
            | XteeName "ametnik"
            | XteeName "id"
            | XteeName "nimi"
            | XteeName "toimik"
            | XteeName "allasutus"
            | XteeName "amet"
            | XteeName "ametniknimi"
            | XteeName "asynkroonne"
            | XteeName "autentija"
            | XteeName "makstud"
            | XteeName "salastada"
            | XteeName "salastada_sertifikaadiga"
            | XteeName "salastatud"
            | XteeName "salastatud_sertifikaadiga" -> Some(name)
            | _ -> None

        /// Matches X-Road header elements.
        let (|XRoadHeader|_|) name =
            match name with
            | XrdName "consumer"
            | XrdName "producer"
            | XrdName "userId"
            | XrdName "id"
            | XrdName "service"
            | XrdName "issue"
            | XrdName "unit"
            | XrdName "position"
            | XrdName "userName"
            | XrdName "async"
            | XrdName "authenticator"
            | XrdName "paid"
            | XrdName "encrypt"
            | XrdName "encryptCert"
            | XrdName "encrypted"
            | XrdName "encryptedCert" -> Some(name)
            | _ -> None

    /// Extracts optional attribute value from current element.
    /// Returns None if attribute is missing.
    let attr (name: XName) (element: XElement) =
        match element.Attribute(name) with
        | null -> None
        | attr -> Some attr.Value

    /// Extracts optional attribute value from current element.
    /// Return default value if attribute is missing.
    let attrOrDefault name value element =
        element |> attr name |> MyOption.defaultValue value

    /// Extracts value of required attribute from current element.
    /// When attribute is not found, exception is thrown.
    let reqAttr (name: XName) (element: XElement) =
        match element.Attribute name with
        | null -> failwithf "Element %A attribute %A is required!" element.Name name
        | attr -> attr.Value

    /// Check if given node is constrained to use qualified form.
    /// Returns true if node requires qualified name.
    let isQualified attrName node =
        match node |> attrOrDefault attrName "unqualified" with
        | "qualified" -> true
        | "unqualified" -> false
        | x -> failwithf "Unknown %s value '%s'" attrName.LocalName x

    /// Parse qualified name from given string.
    let parseXName (element: XElement) (qualifiedName: string) =
        match qualifiedName.Split(':') with
        | [| name |] -> X.name name <| element.GetDefaultNamespace().NamespaceName
        | [| prefix; name |] -> X.name name <| element.GetNamespaceOfPrefix(prefix).NamespaceName
        | _ -> failwithf "Invalid qualified name string %s" qualifiedName

    /// Check if given uri is valid network location or file path in local file system.
    let resolveUri uri =
        match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
        | true -> Uri(uri, UriKind.Absolute)
        | _ ->
            let fullPath = (FileInfo(uri)).FullName
            match File.Exists(fullPath) with
            | true -> Uri(fullPath)
            | _ -> failwith (sprintf "Cannot resolve url location `%s`" uri)

    /// Globally unique identifier for Xml Schema elements and types.
    type SchemaName =
        | SchemaElement of XName
        | SchemaType of XName
        member this.XName
            with get() =
                match this with
                | SchemaElement(name)
                | SchemaType(name) -> name
        override this.ToString() =
            match this with
            | SchemaElement(name) -> sprintf "SchemaElement(%A)" name
            | SchemaType(name) -> sprintf "SchemaType(%A)" name

    /// WSDL and SOAP binding style.
    type BindingStyle =
        | Document
        | Rpc
        static member FromNode(node, ?defValue) =
            match node |> attr (X.lname "style") with
            | Some("document") -> Document
            | Some("rpc") -> Rpc
            | Some(v) -> failwithf "Unknown binding style value `%s`" v
            | None -> defaultArg defValue Document

    /// Service method parameters for X-Road operations.
    type Parameter =
        { Name: XName
          Type: XName option }

    /// Combines parameter for request or response.
    type OperationContent =
        { HasMultipartContent: bool
          Parameters: Parameter list
          RequiredHeaders: string list }

    /// Type that represents different style of message formats.
    type MethodCall =
        // Encoded always type
        | RpcEncoded of XName * OperationContent
        // Element directly under accessor element (named after message part name).
        // Type becomes the schema type of part accessor element.
        | RpcLiteral of XName * OperationContent
        // Encoded uses always type attribues.
        | DocEncoded of XNamespace * OperationContent
        // Element directly under body.
        // Type becomes the schema type of enclosing element (Body)
        | DocLiteral of OperationContent
        // Document literal with type part which defines SOAP:Body
        | DocLiteralBody of OperationContent
        // Recognizes valid document literal wrapped style operations.
        | DocLiteralWrapped of XName * OperationContent
        member this.IsEncoded =
            match this with RpcEncoded(_) | DocEncoded(_) -> true | _ -> false
        member this.Content =
            match this with
            | RpcEncoded(_,content) | RpcLiteral(_,content)
            | DocEncoded(_,content) | DocLiteral(content)
            | DocLiteralWrapped(_,content) | DocLiteralBody(content) -> content
        member this.RequiredHeaders =
            this.Content.RequiredHeaders
        member this.IsMultipart =
            this.Content.HasMultipartContent
        member this.Parameters =
            this.Content.Parameters

    /// Definition for method which corresponds to single X-Road operation.
    type ServicePortMethod =
        { Name: string
          Version: string option
          InputParameters: MethodCall
          OutputParameters: MethodCall
          Documentation: string option }

    /// Collects multiple operations into logical group.
    type ServicePort =
        { Name: string
          Documentation: string option
          Uri: string
          Methods: ServicePortMethod list
          MessageProtocol: XRoadMessageProtocolVersion }

    /// All operations defined for single producer.
    type Service =
        { Name: string
          Ports: ServicePort list
          Namespace: XNamespace }
