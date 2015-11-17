module internal XRoad.Common

open System
open System.IO
open System.Xml.Linq

[<AutoOpen>]
module Option =
    /// Use default value in case of None.
    let orDefault value opt =
        opt |> Option.fold (fun _ t -> t) value

/// Defines namespaces which are handled separately (not generated).
let predefinedNamespaces = [
    XmlNamespace.Http
    XmlNamespace.Mime
    XmlNamespace.Soap
    XmlNamespace.SoapEnc
    XmlNamespace.SoapEnv
    XmlNamespace.Wsdl
    XmlNamespace.Xmime
    XmlNamespace.Xml
    XmlNamespace.Xsd
    XmlNamespace.Xsi
]

/// Active patterns for matching XML document nodes from various namespaces.
[<AutoOpen>]
module Pattern =
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
        | XmlNamespace.XRoad -> Some name.LocalName
        | _ -> None

    /// Matches names defined in `http://x-tee.riik.ee/xsd/xtee.xsd` namespace.
    let (|XteeName|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.Xtee -> Some name.LocalName
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
        | XsdName "date" -> Some typeof<DateTime>
        | XsdName "dateTime" -> Some typeof<DateTime>
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

/// Helper function for generating XNamespace-s.
let xns name = XNamespace.Get(name)

/// Helper function for generating XName-s.
let xname name = XName.Get(name)

/// Helper function for generating XName-s with namespace qualifier.
let xnsname name ns = XName.Get(name, ns)

let (%!) (e: XElement) (xn: XName) = e.Element(xn)
let (%*) (e: XElement) (xn: XName) = e.Elements(xn)

/// Extracts optional attribute value from current element.
/// Returns None if attribute is missing.
let attr (name: XName) (element: XElement) =
    match element.Attribute(name) with
    | null -> None
    | attr -> Some attr.Value

/// Extracts optional attribute value from current element.
/// Return default value if attribute is missing.
let attrOrDefault name value element =
    element |> attr name |> Option.orDefault value

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
    | [| name |] -> xnsname name <| element.GetDefaultNamespace().NamespaceName
    | [| prefix; name |] -> xnsname name <| element.GetNamespaceOfPrefix(prefix).NamespaceName
    | _ -> failwithf "Invalid qualified name string %s" qualifiedName

/// Check if given uri is valid network location or file path in local file system.
let resolveUri uri =
    match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
    | true -> uri
    | _ ->
        let fullPath = (new FileInfo(uri)).FullName
        match File.Exists(fullPath) with
        | true -> fullPath
        | _ -> failwith (sprintf "Cannot resolve url location `%s`" uri)

/// Define X-Road SOAP header element name and description values depending on operation style used in WSDL binding:
/// First tuple contains RPC/Encoded style values and second one values for Document/Literal style.
let headerMapping = function
    | "asutus"                    -> ("asutus", "Asutus", "Asutuse DNS-nimi."),
                                     ("consumer", "Consumer", "DNS-name of the institution")
    | "andmekogu"                 -> ("andmekogu", "Andmekogu", "Andmekogu DNS-nimi."),
                                     ("producer", "Producer", "DNS-name of the database")
    | "isikukood"                 -> ("isikukood", "Isikukood", "Teenuse kasutaja isikukood, millele eelneb kahekohaline maa kood. Näiteks EE37702026518."),
                                     ("userId", "UserId", "ID code of the person invoking the service, preceded by a two-letter country code. For example: EE37702026518")
    | "ametnik"                   -> ("ametnik", "Ametnik", "Teenuse kasutaja Eesti isikukood (ei ole kasutusel alates versioonist 5.0)."),
                                     ("", "", "")
    | "id"                        -> ("id", "Id", "Teenuse väljakutse nonss (unikaalne identifikaator)."),
                                     ("id", "Id", "Service invocation nonce (unique identifier)")
    | "nimi"                      -> ("nimi", "Nimi", "Kutsutava teenuse nimi."),
                                     ("service", "Service", "Name of the service to be invoked")
    | "toimik"                    -> ("toimik", "Toimik", "Teenuse väljakutsega seonduva toimiku number (mittekohustuslik)."),
                                     ("issue", "Issue", "Name of file or document related to the service invocation")
    | "allasutus"                 -> ("allasutus", "Allasutus", "Asutuse registrikood, mille nimel teenust kasutatakse (kasutusel juriidilise isiku portaalis)."),
                                     ("unit", "Unit", "Registration code of the institution or its unit on whose behalf the service is used (applied in the legal entity portal)")
    | "amet"                      -> ("amet", "Amet", "Teenuse kasutaja ametikoht."),
                                     ("position", "Position", "Organizational position or role of the person invoking the service")
    | "ametniknimi"               -> ("ametniknimi", "Ametniknimi", "Teenuse kasutaja nimi."),
                                     ("userName", "UserName", "Name of the person invoking the service")
    | "asynkroonne"               -> ("asynkroonne", "Asynkroonne", "Teenuse kasutamise asünkroonsus. Kui väärtus on 'true', siis sooritab turvaserver päringu asünkroonselt."),
                                     ("async", "Async", "Specifies asynchronous service. If the value is \"true\", then the security server performs the service call asynchronously.")
    | "autentija"                 -> ("autentija", "Autentija", "Teenuse kasutaja autentimise viis. Võimalikud variandid on: ID - ID-kaardiga autenditud; SERT - muu sertifikaadiga autenditud; PANK - panga kaudu autenditud; PAROOL - kasutajatunnuse ja parooliga autenditud. Autentimise viisi järel võib sulgudes olla täpsustus (näiteks panga kaudu autentimisel panga tunnus infosüsteemis)."),
                                     ("authenticator", "Authenticator", "Authentication method, one of the following: ID-CARD - with a certificate of identity; CERT - with another certificate; EXTERNAL - through a third-party service; PASSWORD - with user ID and a password. Details of the authentication (e.g. the identification of a bank for external authentication) can be given in brackets after the authentication method.")
    | "makstud"                   -> ("makstud", "Makstud", "Teenuse kasutamise eest makstud summa."),
                                     ("paid", "Paid", "The amount of money paid for invoking the service")
    | "salastada"                 -> ("salastada", "Salastada", "Kui asutusele on X-tee keskuse poolt antud päringute salastamise õigus ja andmekogu on nõus päringut salastama, siis selle elemendi olemasolul päringu päises andmekogu turvaserver krüpteerib päringu logi, kasutades selleks X-tee keskuse salastusvõtit."),
                                     ("encrypt", "Encrypt", "If an organization has got the right from the X-Road Center to hide queries, with the database agreeing to hide the query, the occurrence of this tag in the query header makes the database security server to encrypt the query log, using the encryption key of the X-Road Center")
    | "salastada_sertifikaadiga"  -> ("salastada_sertifikaadiga", "SalastadaSertifikaadiga", "Päringu sooritaja ID-kaardi autentimissertifikaat DERkujul base64 kodeerituna. Selle elemendi olemasolu päringu päises väljendab soovi päringu logi salastamiseks asutuse turvaserveris päringu sooritaja ID-kaardi autentimisvõtmega. Seda välja kasutatakse ainult kodaniku päringute portaalis."),
                                     ("encryptCert", "EncryptCert", "Authentication certificate of the query invokers ID Card, in the base64-encoded DER format. Occurrence of this tag in the query header represents the wish to encrypt the query log in the organizations security server, using authentication key of the query invokers ID Card. This field is used in the Citizen Query Portal only.")
    | "salastatud"                -> ("salastatud", "Salastatud", "Kui päringu välja päises oli element salastada ja päringulogi salastamine õnnestus, siis vastuse päisesse lisatakse tühi element salastatud."),
                                     ("encrypted", "Encrypted", "If the query header contains the encrypt tag and the query log as been successfully encrypted, an empty encrypted tag will be inserted in the reply header.")
    | "salastatud_sertifikaadiga" -> ("salastatud_sertifikaadiga", "SalastatudSertifikaadiga", "Kui päringu päises oli element salastada_sertifikaadiga ja päringulogi salastamine õnnestus, siis vastuse päisesesse lisatakse tühi element salastatud_sertifikaadiga."),
                                     ("encryptedCert", "EncryptedCert", "If the query header contains the encryptedCert tag and the query log has been successfully encrypted, an empty encryptedCert tag will accordingly be inserted in the reply header.")
    | name                        -> failwithf "Invalid header name '%s'" name

// Helper functions to extract values from three-argument tuples.
let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let trd3 (_, _, x) = x

/// Globally unique identifier for Xml Schema elements and types.
type SchemaName =
    | SchemaElement of XName
    | SchemaType of XName
    member this.XName
        with get() =
            match this with
            | SchemaElement(name)
            | SchemaType(name) -> name

[<AutoOpen>]
module CodeSpec =
    /// X-Road protocol version.
    type XRoadProtocol =
        | Version_20
        | Version_30
        | Version_31
        | Version_40
        member this.Name =
            match this with
            | Version_20 -> "Version20"
            | Version_30 -> "Version30"
            | Version_31 -> "Version31"
            | Version_40 -> "Version40"
        /// Returns namespace value for the protocol version.
        member this.Namespace =
            match this with
            | Version_20 -> XmlNamespace.Xtee
            | Version_30 -> XmlNamespace.Xrd
            | Version_31 -> XmlNamespace.XRoad
            | Version_40 -> "X-Road protocol version 4.0 is not implemented."
        /// Extracts X-Road protocol version from namespace that is used.
        static member FromNamespace(ns) =
            match ns with
            | XmlNamespace.Xtee -> Version_20
            | XmlNamespace.Xrd -> Version_30
            | XmlNamespace.XRoad -> Version_31
            | _ -> failwithf "Unexpected X-Road namespace value `%s`." ns

    /// WSDL and SOAP binding style.
    type BindingStyle =
        | Document
        | Rpc
        static member FromNode(node, ?defValue) =
            match node |> attr (xname "style") with
            | Some("document") -> Document
            | Some("rpc") -> Rpc
            | Some(v) -> failwithf "Unknown binding style value `%s`" v
            | None -> defaultArg defValue Document

    /// Service method parameters for X-Road operations.
    type Parameter =
        { Name: string
          Type: SchemaName }

    /// Combines parameter for request or response.
    type ParameterWrapper =
        { HasMultipartContent: bool
          Parameters: Parameter list
          RequiredHeaders: string list }

    /// Type that represents different style of message formats.
    type MethodCall =
        // Encoded always type
        | RpcEncodedCall of accessorName: XName * parameters: ParameterWrapper
        // Element directly under accessor element (named after message part name).
        // Type becomes the schema type of part accessor element.
        | RpcLiteralCall of accessorName: XName * parameters: ParameterWrapper
        // Encoded uses always type attribues.
        | DocEncodedCall of ns: XNamespace * parameters: ParameterWrapper
        // Element directly under body.
        // Type becomes the schema type of enclosing element (Body)
        | DocLiteralCall of parameters: ParameterWrapper
        member this.Accessor =
            match this with
            | DocEncodedCall(_) | DocLiteralCall(_) -> None
            | RpcEncodedCall(accessor, _) | RpcLiteralCall(accessor, _) -> Some(accessor)
        member this.Wrapper =
            match this with
            | DocEncodedCall(_, wrapper)
            | DocLiteralCall(wrapper)
            | RpcEncodedCall(_, wrapper)
            | RpcLiteralCall(_, wrapper) -> wrapper
        member this.IsEncoded =
            match this with RpcEncodedCall(_) -> true | _ -> false
        member this.RequiredHeaders =
            this.Wrapper.RequiredHeaders
        member this.IsMultipart =
            this.Wrapper.HasMultipartContent
        member this.Parameters =
            this.Wrapper.Parameters

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
          Producer: string
          Methods: ServicePortMethod list
          Protocol: XRoadProtocol }

    /// All operations defined for single producer.
    type Service =
        { Name: string
          Ports: ServicePort list }
