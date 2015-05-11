module internal XRoad.Common

open System
open System.IO
open System.Xml.Linq

[<AutoOpen>]
module Option =
    /// Use default value in case of None.
    let orDefault value opt =
        opt |> Option.fold (fun _ t -> t) value

/// Xml namespaces which are used in almost every X-Road service description.
module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"

    /// Defines namespaces which are handled separately (not generated).
    let predefined = [ Http; Mime; Soap; SoapEnc; SoapEnv; Wsdl; Xmime; Xml; Xsd; Xsi ]

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
        | XsdName "int" -> Some typeof<int>
        | XsdName "integer" -> Some typeof<bigint>
        | XsdName "long" -> Some typeof<int64>
        | XsdName "string" -> Some typeof<string>
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
