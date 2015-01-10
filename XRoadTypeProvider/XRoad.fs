module XRoadTypeProvider.XRoad

open System.Web.Services.Description
open System.Xml

[<Literal>]
let XRoadOldNamespace = "http://x-tee.riik.ee/xsd/xtee.xsd"

[<Literal>]
let XRoadNewNamespace = "http://x-road.ee/xsd/x-road.xsd"

let GetOperationVersion (operation : OperationBinding) =
    [ for ext in operation.Extensions -> ext ]
    |> Seq.choose (fun extension ->
        match extension with
        | :? XmlElement as element ->
            match (element.NamespaceURI, element.LocalName) with
            | (XRoadOldNamespace, "version")
            | (XRoadNewNamespace, "version") -> Some element.InnerText
            | _ -> None
        | _ -> None)
    |> Seq.exactlyOne

type Base64 = string

/// Elements of the SOAP:Header component
type XRoadHeader () =
    /// DNS-name of the institution
    member val Consumer: string option = None with get, set

    /// DNS-name of the database
    member val Producer: string option = None with get, set

    /// ID code of the person invoking the service, preceded by a two-letter country code.
    /// For example: EE37702026518
    member val UserId: string option = None with get, set

    /// Service invocation nonce (unique identifier)
    member val Id: string option = None with get, set

    /// Name of the service to be invoked
    member val Service: string option = None with get, set

    /// Name of file or document related to the service invocation
    member val Issue: string option = None with get, set

    /// Registration code of the institution or its unit on whose behalf the service is used
    /// (applied in the legal entity portal)
    member val Unit: string option = None with get, set

    /// Organizational position or role of the person invoking the service
    member val Position: string option = None with get, set

    /// Name of the person invoking the service
    member val UserName: string option = None with get, set

    /// Specifies asynchronous service. If the value is "true", then the security server performs
    /// the service call asynchronously.
    member val Async: bool option = None with get, set

    /// Authentication method, one of the following:
    /// * ID-CARD - with a certificate of identity
    /// * CERT - with another certificate
    /// * EXTERNAL - through a third-party service
    /// * PASSWORD - with user ID and a password
    /// Details of the authentication (e.g. the identification of a bank for external authentication)
    /// can be given in brackets after the authentication method.
    member val Authenticator: string option = None with get, set

    /// The amount of money paid for invoking the service
    member val Paid: string option = None with get, set

    /// If an organization has got the right from the X-Road Center to hide queries, with the database
    /// agreeing to hide the query, the occurrence of this tag in the query header makes the database
    /// security server to encrypt the query log, using the encryption key of the X-Road Center
    member val Encrypt: string option = None with get, set

    /// Authentication certificate of the query invokers ID Card, in the base64-encoded DER format.
    /// Occurrence of this tag in the query header represents the wish to encrypt the query log in the
    /// organizations security server, using authentication key of the query invokers ID Card.
    /// This field is used in the Citizen Query Portal only.
    member val EncryptCert: Base64 option = None with get, set

    /// If the query header contains the encrypt tag and the query log as been successfully encrypted,
    /// an empty encrypted tag will be inserted in the reply header.
    member val Encrypted: string option = None with get, set

    /// If the query header contains the encryptedCert tag and the query log has been successfully encrypted,
    /// an empty encryptedCert tag will accordingly be inserted in the reply header.
    member val EncryptedCert: string option = None with get, set
