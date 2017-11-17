namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Xml
open System.Reflection
open XRoad.Serialization.Attributes

[<RequireQualifiedAccessAttribute>]
module internal XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xop = "http://www.w3.org/2004/08/xop/include"
    let [<Literal>] XRoad20 = "http://x-tee.riik.ee/xsd/xtee.xsd"
    let [<Literal>] XRoad30 = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad31Ee = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] XRoad31Eu = "http://x-road.eu/xsd/x-road.xsd"
    let [<Literal>] XRoad40 = "http://x-road.eu/xsd/xroad.xsd"
    let [<Literal>] XRoad40Id = "http://x-road.eu/xsd/identifiers"
    let [<Literal>] XRoad40Repr = "http://x-road.eu/xsd/representation.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"

    /// Defines namespaces which are handled separately (not generated).
    let predefined =
        [ Http; Mime; Soap; SoapEnc; SoapEnv; Wsdl; Xmime; Xml; Xmlns; Xop; Xsd; Xsi; XRoad40; XRoad40Id; XRoad40Repr ]

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
    open System.Xml.Linq

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

module XRoadHelper =
    let getUUID () = System.Guid.NewGuid().ToString()

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

type internal ContentType =
    | FileStorage of FileInfo
    | Data of byte[]

type public ContentEncoding =
    | Binary = 0
    | Base64 = 1

[<AbstractClass; AllowNullLiteral>]
type public AbstractXRoadHeader() =
    /// Unresolved header elements.
    member val Unresolved = List<System.Xml.Linq.XElement>() with get, set

/// Combines X-Road SOAP headers for RPC style messages.
[<AllowNullLiteral>]
type public XRoadRpcHeader() =
    inherit AbstractXRoadHeader()
    // Copy-constructor which initializes new header instance based on given argument.
    new (other: XRoadRpcHeader) as this = XRoadRpcHeader() then
        if not (other |> isNull) then
            this.Allasutus <- other.Allasutus
            this.Amet <- other.Amet
            this.Ametnik <- other.Ametnik
            this.AmetnikNimi <- other.AmetnikNimi
            this.Andmekogu <- other.Andmekogu
            this.Asutus <- other.Asutus
            this.Asynkroonne <- other.Asynkroonne
            this.Autentija <- other.Autentija
            this.Id <- other.Id
            this.Isikukood <- other.Isikukood
            this.Makstud <- other.Makstud
            this.Salastada <- other.Salastada
            this.SalastadaSertifikaadiga <- other.SalastadaSertifikaadiga
            this.Salastatud <- other.Salastatud
            this.SalastatudSertifikaadiga <- other.SalastatudSertifikaadiga
            this.Toimik <- other.Toimik
            this.Unresolved <- other.Unresolved
    /// Asutuse DNS-nimi.
    member val Asutus = "" with get, set
    /// Andmekogu DNS-nimi.
    member val Andmekogu = "" with get, set
    /// Teenuse kasutaja isikukood, millele eelneb kahekohaline maa kood. Näiteks EE37702026518.
    member val Isikukood = "" with get, set
    /// Teenuse kasutaja Eesti isikukood (ei ole kasutusel alates versioonist 5.0).
    member val Ametnik = "" with get, set
    /// Teenuse väljakutse nonss (unikaalne identifikaator).
    member val Id = "" with get, set
    /// Teenuse väljakutsega seonduva toimiku number (mittekohustuslik).
    member val Toimik = "" with get, set
    /// Asutuse registrikood, mille nimel teenust kasutatakse (kasutusel juriidilise isiku portaalis).
    member val Allasutus = "" with get, set
    /// Teenuse kasutaja ametikoht.
    member val Amet = "" with get, set
    /// Teenuse kasutaja nimi.
    member val AmetnikNimi = "" with get, set
    /// Teenuse kasutamise asünkroonsus. Kui väärtus on "true", siis sooritab turvaserver päringu asünkroonselt.
    member val Asynkroonne = Unchecked.defaultof<Nullable<bool>> with get, set
    /// Teenuse kasutaja autentimise viis. Võimalikud variandid on: ID - ID-kaardiga autenditud; SERT - muu sertifikaadiga autenditud; PANK - panga kaudu autenditud; PAROOL - kasutajatunnuse ja parooliga autenditud. Autentimise viisi järel võib sulgudes olla täpsustus (näiteks panga kaudu autentimisel panga tunnus infosüsteemis).
    member val Autentija = "" with get, set
    /// Teenuse kasutamise eest makstud summa.
    member val Makstud = "" with get, set
    /// Kui asutusele on X-tee keskuse poolt antud päringute salastamise õigus ja andmekogu on nõus päringut salastama, siis selle elemendi olemasolul päringu päises andmekogu turvaserver krüpteerib päringu logi, kasutades selleks X-tee keskuse salastusvõtit.
    member val Salastada = "" with get, set
    /// Päringu sooritaja ID-kaardi autentimissertifikaat DERkujul base64 kodeerituna. Selle elemendi olemasolu päringu päises väljendab soovi päringu logi salastamiseks asutuse turvaserveris päringu sooritaja ID-kaardi autentimisvõtmega. Seda välja kasutatakse ainult kodaniku päringute portaalis."
    member val SalastadaSertifikaadiga = ""B with get, set
    /// Kui päringu välja päises oli element salastada ja päringulogi salastamine õnnestus, siis vastuse päisesse lisatakse tühi element salastatud.
    member val Salastatud = "" with get, set
    /// Kui päringu päises oli element salastada_sertifikaadiga ja päringulogi salastamine õnnestus, siis vastuse päisesesse lisatakse tühi element salastatud_sertifikaadiga.
    member val SalastatudSertifikaadiga = "" with get, set

/// Combines X-Road SOAP headers for document style messages.
[<AllowNullLiteral>]
type public XRoadDocHeader() =
    inherit AbstractXRoadHeader()
    // Copy-constructor which initializes new header instance based on given argument.
    new (other: XRoadDocHeader) as this = XRoadDocHeader() then
        if not (other |> isNull) then
            this.Async <- other.Async
            this.Authenticator <- other.Authenticator
            this.Consumer <- other.Consumer
            this.Encrypt <- other.Encrypt
            this.EncryptCert <- other.EncryptCert
            this.Encrypted <- other.Encrypted
            this.EncryptedCert <- other.EncryptedCert
            this.Id <- other.Id
            this.Issue <- other.Issue
            this.Paid <- other.Paid
            this.Position <- other.Position
            this.Producer <- other.Producer
            this.Unit <- other.Unit
            this.Unresolved <- other.Unresolved
            this.UserId <- other.UserId
            this.UserName <- other.UserName
    /// DNS-name of the institution
    member val Consumer = "" with get, set
    /// DNS-name of the database
    member val Producer = "" with get, set
    /// ID code of the person invoking the service, preceded by a two-letter country code. For example: EE37702026518.
    member val UserId = "" with get, set
    /// Service invocation nonce (unique identifier).
    member val Id = "" with get, set
    /// Name of file or document related to the service invocation.
    member val Issue = "" with get, set
    /// Registration code of the institution or its unit on whose behalf the service is used (applied in the legal entity portal).
    member val Unit = "" with get, set
    /// Organizational position or role of the person invoking the service.
    member val Position = "" with get, set
    /// Name of the person invoking the service.
    member val UserName = "" with get, set
    /// Specifies asynchronous service. If the value is "true", then the security server performs the service call asynchronously.
    member val Async = Unchecked.defaultof<Nullable<bool>> with get, set
    /// Authentication method, one of the following: ID-CARD - with a certificate of identity; CERT - with another certificate; EXTERNAL - through a third-party service; PASSWORD - with user ID and a password. Details of the authentication (e.g. the identification of a bank for external authentication) can be given in brackets after the authentication method.
    member val Authenticator = "" with get, set
    /// The amount of money paid for invoking the service.
    member val Paid = "" with get, set
    /// If an organization has got the right from the X-Road Center to hide queries, with the database agreeing to hide the query, the occurrence of this tag in the query header makes the database security server to encrypt the query log, using the encryption key of the X-Road Center.
    member val Encrypt = "" with get, set
    /// Authentication certificate of the query invokers ID Card, in the base64-encoded DER format. Occurrence of this tag in the query header represents the wish to encrypt the query log in the organizations security server, using authentication key of the query invokers ID Card. This field is used in the Citizen Query Portal only.
    member val EncryptCert = ""B with get, set
    /// If the query header contains the encrypt tag and the query log as been successfully encrypted, an empty encrypted tag will be inserted in the reply header.
    member val Encrypted = "" with get, set
    /// If the query header contains the encryptedCert tag and the query log has been successfully encrypted, an empty encryptedCert tag will accordingly be inserted in the reply header.
    member val EncryptedCert = "" with get, set

/// Represents identifiers that can be used by the service clients, namely X-Road members and subsystems.
[<AllowNullLiteral>]
type public XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) =
    new () = XRoadMemberIdentifier("", "", "", "")
    new (xRoadInstance, memberClass, memberCode) = XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, "")
    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = xRoadInstance with get, set
    /// Code identifying the member class (e.g., government agency, private enterprise, physical person).
    member val MemberClass = memberClass with get, set
    /// Member code that uniquely identifies the given X-Road member within its member class.
    member val MemberCode = memberCode with get, set
    /// Subsystem code is chosen by the X-Road member and it must be unique among the subsystems of this member.
    member val SubsystemCode = subsystemCode with get, set

/// Represents identifiers of services.
[<AllowNullLiteral>]
type public XRoadServiceIdentifier() =
    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = "" with get, set
    /// Code identifying the member class (e.g., government agency, private enterprise, physical person).
    member val MemberClass = "" with get, set
    /// Member code that uniquely identifies the given X-Road member within its member class.
    member val MemberCode = "" with get, set
    /// Subsystem code is chosen by the X-Road member and it must be unique among the subsystems of this member.
    member val SubsystemCode = "" with get, set
    /// The service code is chosen by the service provider.
    member val ServiceCode = "" with get, set
    /// Version is optional and can be used to distinguish between technically incompatible versions of the same basic service.
    member val ServiceVersion = "" with get, set

/// Represents identifiers of central services.
[<AllowNullLiteral>]
type public XRoadCentralServiceIdentifier() =
    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = "" with get, set
    /// The service code is chosen by the service provider.
    member val ServiceCode = "" with get, set

/// Combines X-Road SOAP headers for X-Road v6.
[<AllowNullLiteral>]
type public XRoadHeader() =
    inherit AbstractXRoadHeader()
    // Copy-constructor which initializes new header instance based on given argument.
    new (other: XRoadHeader) as this = XRoadHeader() then
        if not (other |> isNull) then
            this.CentralService <- other.CentralService
            this.Client <- other.Client
            this.Id <- other.Id
            this.Issue <- other.Issue
            this.Producer <- other.Producer
            this.ProtocolVersion <- other.ProtocolVersion
            this.RequestHash <- other.RequestHash
            this.RequestHashAlgorithm <- other.RequestHashAlgorithm
            this.Unresolved <- other.Unresolved
            this.UserId <- other.UserId
    /// Identifies a service client – an entity that initiates the service call.
    member val Client = new XRoadMemberIdentifier() with get, set
    /// Identifies the service that is invoked by the request.
    member val Producer = new XRoadMemberIdentifier() with get, set
    /// Identifies the central service that is invoked by the request.
    member val CentralService = new XRoadCentralServiceIdentifier() with get, set
    /// Unique identifier for this message. The recommended form of message ID is UUID.
    member val Id = "" with get, set
    /// User whose action initiated the request. The user ID should be prefixed with two-letter ISO country code (e.g., EE12345678901).
    member val UserId = "" with get, set
    /// For responses, this field contains a Base64 encoded hash of the request SOAP message.
    /// This field is automatically filled in by the service provider's security server.
    member val RequestHash = "" with get, set
    /// Identifies the hash algorithm that was used to calculate the value of the requestHash field.
    /// The algorithms are specified as URIs listed in the XML-DSIG specification [DSIG].
    member val RequestHashAlgorithm = "" with get, set
    /// Identifies received application, issue or document that was the cause of the service request.
    /// This field may be used by the client information system to connect service requests (and responses) to working procedures.
    member val Issue = "" with get, set
    /// X-Road message protocol version. The value of this field MUST be 4.0
    member val ProtocolVersion = "" with get, set

[<AllowNullLiteral>]
type public BinaryContent internal (contentID: string, content: ContentType) =
    member val ContentEncoding = ContentEncoding.Binary with get, set
    member __.ContentID
        with get() =
            match contentID with
            | null | "" -> XRoadHelper.getUUID()
            | _ -> contentID
    member __.OpenStream() : Stream =
        match content with
        | FileStorage(file) -> upcast file.OpenRead()
        | Data(data) -> upcast new MemoryStream(data)
    member __.GetBytes() =
        match content with
        | FileStorage(file) -> File.ReadAllBytes(file.FullName)
        | Data(data) -> data
    static member Create(file) = BinaryContent("", FileStorage(file))
    static member Create(contentID, file) = BinaryContent(contentID, FileStorage(file))
    static member Create(data) = BinaryContent("", Data(data))
    static member Create(contentID, data) = BinaryContent(contentID, Data(data))

[<AllowNullLiteral>]
type SerializerContext() =
    let attachments = Dictionary<string, BinaryContent>()
    member val IsMultipart = false with get, set
    member val Attachments = attachments with get
    member this.AddAttachments(attachments: IDictionary<_,_>) =
        match attachments with
        | null -> ()
        | _ -> attachments |> Seq.iter (fun kvp -> this.Attachments.Add(kvp.Key, kvp.Value))
    member __.GetAttachment(href: string) =
        if href.StartsWith("cid:") then
            let contentID = href.Substring(4)
            match attachments.TryGetValue(contentID) with
            | true, value -> value
            | _ -> failwithf "Multipart message doesn't contain content part with ID `%s`." contentID
        else failwithf "Invalid multipart content reference: `%s`." href

type MethodPartMap =
    { IsEncoded: bool
      IsMultipart: bool
      Accessor: XmlQualifiedName option }

type OperationDeserializerDelegate = delegate of XmlReader * SerializerContext -> obj
type OperationSerializerDelegate = delegate of XmlWriter * obj[] * SerializerContext -> unit

type MethodMap =
    { Deserializer: OperationDeserializerDelegate
      Serializer: OperationSerializerDelegate
      Protocol: XRoadProtocol
      Request: MethodPartMap
      Response: MethodPartMap
      ServiceCode: string
      ServiceVersion: string option
      Namespaces: string list
      RequiredHeaders: IDictionary<string, string[]> }

type SoapHeaderValue(name: XmlQualifiedName, value: obj, required: bool) =
    member val Name = name with get
    member val Value = value with get
    member val IsRequired = required with get

module internal Wsdl =
    open System.Xml.Linq

    /// Helper function for generating XNamespace-s.
    let xns name = XNamespace.Get(name)

    /// Helper function for generating XName-s.
    let xname name = XName.Get(name)

    /// Helper function for generating XName-s with namespace qualifier.
    let xnsname name ns = XName.Get(name, ns)

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
        element |> attr name |> Option.defaultValue value

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
            match node |> attr (xname "style") with
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


module internal MultipartMessage =
    open System.Net
    open System.Text

    type private ChunkState = Limit | NewLine | EndOfStream

    type private PeekStream(stream: Stream) =
        let mutable borrow = None : int option
        member __.Read() =
            match borrow with
            | Some(x) ->
                borrow <- None
                x
            | None -> stream.ReadByte()
        member __.Peek() =
            match borrow with
            | None ->
                let x = stream.ReadByte()
                borrow <- Some(x)
                x
            | Some(x) -> x
        member __.Flush() = stream.Flush()
        interface IDisposable with
            member __.Dispose() =
                stream.Dispose()

    let private getBoundaryMarker (response: WebResponse) =
        let parseMultipartContentType (contentType: string) =
            let parts = contentType.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
                        |> List.ofArray
                        |> List.map (fun x -> x.Trim())
            match parts with
            | "multipart/related" :: parts ->
                parts |> List.tryFind (fun x -> x.StartsWith("boundary="))
                      |> Option.map (fun x -> x.Substring(9).Trim('"'))
            | _ -> None
        response
        |> Option.ofObj
        |> Option.map (fun r -> r.ContentType)
        |> Option.bind (parseMultipartContentType)

    let [<Literal>] private CHUNK_SIZE = 4096
    let [<Literal>] private CR = 13
    let [<Literal>] private LF = 10

    let private readChunkOrLine (buffer: byte []) (stream: PeekStream) =
        let rec addByte pos =
            if pos >= CHUNK_SIZE then (ChunkState.Limit, pos)
            else
                match stream.Read() with
                | -1 -> (ChunkState.EndOfStream, pos)
                | byt ->
                    if byt = CR && stream.Peek() = LF then
                        stream.Read() |> ignore
                        (ChunkState.NewLine, pos)
                    else
                        buffer.[pos] <- Convert.ToByte(byt)
                        addByte (pos + 1)
        let result = addByte 0
        stream.Flush()
        result

    let private readLine stream =
        let mutable line = [| |] : byte []
        let buffer = Array.zeroCreate<byte>(CHUNK_SIZE)
        let rec readChunk () =
            let (state, chunkSize) = stream |> readChunkOrLine buffer
            Array.Resize(&line, line.Length + chunkSize)
            Array.Copy(buffer, line, chunkSize)
            match state with
            | ChunkState.Limit -> readChunk()
            | ChunkState.EndOfStream
            | ChunkState.NewLine -> ()
        readChunk()
        line

    let private extractMultipartContentHeaders (stream: PeekStream) =
        let rec getHeaders () = seq {
            match Encoding.ASCII.GetString(stream |> readLine).Trim() with
            | null | "" -> ()
            | line ->
                match line.Split([| ':' |], 2) with
                | [| name |] -> yield (name.Trim(), "")
                | [| name; content |] -> yield (name.Trim(), content.Trim())
                | _ -> failwith "never"
                yield! getHeaders() }
        getHeaders() |> Map.ofSeq

    let private base64Decoder (encoding: Encoding) (encodedBytes: byte []) =
        match encodedBytes with
        | null | [| |] -> [| |]
        | _ ->
            let chars = encoding.GetChars(encodedBytes)
            Convert.FromBase64CharArray(chars, 0, chars.Length)

    let private getDecoder (contentEncoding: string) =
        match contentEncoding.ToLower() with
        | "base64" -> Some(base64Decoder)
        | "quoted-printable" | "7bit" | "8bit" | "binary" -> None
        | _ -> failwithf "No decoder implemented for content transfer encoding `%s`." contentEncoding

    let private startsWith (value: byte []) (buffer: byte []) =
        let rec compare i =
            if value.[i] = buffer.[i] then
                if i = 0 then true else compare (i - 1)
            else false
        if buffer |> isNull || value |> isNull || value.Length > buffer.Length then false
        else compare (value.Length - 1)

    let internal read (response: WebResponse) : Stream * BinaryContent list =
        match response |> getBoundaryMarker with
        | Some(boundaryMarker) ->
            use stream = new PeekStream(response.GetResponseStream())
            let contents = List<string option * MemoryStream>()
            let contentMarker = Encoding.ASCII.GetBytes(sprintf "--%s" boundaryMarker)
            let endMarker = Encoding.ASCII.GetBytes(sprintf "--%s--" boundaryMarker)
            let (|Content|End|Separator|) line =
                if line |> startsWith endMarker then End
                elif line |> startsWith contentMarker then Content
                else Separator
            let buffer = Array.zeroCreate<byte>(CHUNK_SIZE)
            let rec copyChunk addNewLine encoding (decoder: (Encoding -> byte[] -> byte[]) option) (contentStream: Stream) =
                let (state,size) = stream |> readChunkOrLine buffer
                if buffer |> startsWith endMarker then false
                elif buffer |> startsWith contentMarker then true
                elif state = ChunkState.EndOfStream then failwith "Unexpected end of multipart stream."
                else
                    if decoder.IsNone && addNewLine then contentStream.Write([| 13uy; 10uy |], 0, 2)
                    let (decodedBuffer,size) = decoder |> Option.fold (fun (buf,_) func -> let buf = buf |> func encoding
                                                                                           (buf,buf.Length)) (buffer,size)
                    contentStream.Write(decodedBuffer, 0, size)
                    match state with EndOfStream -> false | _ -> copyChunk (state = ChunkState.NewLine) encoding decoder contentStream
            let parseContentPart () =
                let headers = stream |> extractMultipartContentHeaders
                let contentId = headers |> Map.tryFind("content-id") |> Option.map (fun x -> x.Trim().Trim('<', '>'))
                let decoder = headers |> Map.tryFind("content-transfer-encoding") |> Option.bind (getDecoder)
                let contentStream = new MemoryStream()
                contents.Add(contentId, contentStream)
                copyChunk false Encoding.UTF8 decoder contentStream
            let rec parseContent () =
                match stream |> readLine with
                | Content -> if parseContentPart() then parseContent() else ()
                | End -> ()
                | Separator -> parseContent()
            parseContent()
            match contents |> Seq.toList with
            | (_,content)::attachments ->
                (upcast content, attachments
                                 |> List.map (fun (name,stream) ->
                                    use stream = stream
                                    stream.Position <- 0L
                                    BinaryContent.Create(name.Value, stream.ToArray())))
            | _ -> failwith "empty multipart content"
        | None ->
            use stream = response.GetResponseStream()
            let content = new MemoryStream()
            stream.CopyTo(content)
            (upcast content, [])
