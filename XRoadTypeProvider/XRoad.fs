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
type XteeHeader = {
    /// DNS-name of the institution
    Consumer: string

    /// DNS-name of the database
    Producer: string

    /// ID code of the person invoking the service, preceded by a two-letter country code.
    /// For example: EE37702026518
    UserId: string

    /// Service invocation nonce (unique identifier)
    Id: string

    /// Name of the service to be invoked
    Service: string

    /// Name of file or document related to the service invocation
    Issue: string

    /// Registration code of the institution or its unit on whose behalf the service is used
    /// (applied in the legal entity portal)
    Unit: string option

    /// Organizational position or role of the person invoking the service
    Position: string option

    /// Name of the person invoking the service
    UserName: string option

    /// Specifies asynchronous service. If the value is "true", then the security server performs
    /// the service call asynchronously.
    Async: bool option

    /// Authentication method, one of the following:
    /// * ID-CARD - with a certificate of identity
    /// * CERT - with another certificate
    /// * EXTERNAL - through a third-party service
    /// * PASSWORD - with user ID and a password
    /// Details of the authentication (e.g. the identification of a bank for external authentication)
    /// can be given in brackets after the authentication method.
    Authenticator: string option

    /// The amount of money paid for invoking the service
    Paid: string option

    /// If an organization has got the right from the X-Road Center to hide queries, with the database
    /// agreeing to hide the query, the occurrence of this tag in the query header makes the database
    /// security server to encrypt the query log, using the encryption key of the X-Road Center
    Encrypt: string option

    /// Authentication certificate of the query invokers ID Card, in the base64-encoded DER format.
    /// Occurrence of this tag in the query header represents the wish to encrypt the query log in the
    /// organizations security server, using authentication key of the query invokers ID Card.
    /// This field is used in the Citizen Query Portal only.
    EncryptCert: Base64 option

    /// If the query header contains the encrypt tag and the query log as been successfully encrypted,
    /// an empty encrypted tag will be inserted in the reply header.
    Encrypted: string option

    /// If the query header contains the encryptedCert tag and the query log has been successfully encrypted,
    /// an empty encryptedCert tag will accordingly be inserted in the reply header.
    EncryptedCert: string option
}
