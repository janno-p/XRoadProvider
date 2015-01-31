module XRoadTypeProvider.Runtime

open System
open System.Collections.Generic
open System.IO
open System.Xml
open XRoadTypeProvider.Wsdl

[<Interface>]
type IXRoadEntity =
    abstract member TypeName: string * string with get, set
    abstract member HasProperty: string -> bool
    abstract member SetProperty: string * 'T -> unit
    abstract member GetProperty: string -> 'T

type XRoadEntity () =
    let data = Dictionary<string, obj>()

    interface IXRoadEntity with
        override val TypeName = ("", "") with get, set

        override __.HasProperty (name) =
            data.ContainsKey(name)

        override __.SetProperty (name, value) =
            data.[name] <- box value

        override __.GetProperty<'T> (name) =
            match data.TryGetValue(name) with
            | true, value -> unbox value
            | _ -> Unchecked.defaultof<'T>

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

[<Interface>]
type IXRoadContext =
    abstract member Address: string with get, set
    abstract member Producer: string with get, set

type XRoadContext () =
    interface IXRoadContext with
        member val Address = "" with get, set
        member val Producer = "" with get, set

type AttachmentCollection () =
    member __.Add (stream: Stream) =
        true

[<Interface>]
type IXRoadResponseWithAttachments<'T> =
    abstract member Result: 'T with get
    abstract member Attachments: Stream [] with get

module XRoadRequest =
    let generateNonce () =
        let nonce = Array.zeroCreate 42
        let rng = System.Security.Cryptography.RNGCryptoServiceProvider.Create()
        rng.GetNonZeroBytes(nonce)
        Convert.ToBase64String(nonce)

    let initRequest (uri: string) =
        let request = System.Net.WebRequest.Create(uri)
        //request.Timeout <- timeout
        request.Method <- "POST"
        request.ContentType <- sprintf "text/xml; charset=%s" System.Text.Encoding.UTF8.HeaderName
        request.Headers.Set("SOAPAction", "")
        //request.Proxy <- System.Net.WebProxy()
        request

    let writeXRoadHeader ns (writer: XmlWriter) reqhdr name value f =
        match reqhdr |> Array.exists ((=) name), value with
        | false, None -> ()
        | _ ->  writer.WriteStartElement(name, ns)
                if value.IsSome then f(value.Value)
                writer.WriteEndElement()

    let makeXRoadCall (context: IXRoadContext) (opName, opVersion, opNamespace) (settings: XRoadHeader) hf writeBody =
        let producer = defaultArg settings.Producer context.Producer
        let serviceName =
            match settings.Service with
            | Some v -> v
            | _ -> match opVersion with
                   | "" -> sprintf "%s.%s" producer opName
                   | v -> sprintf "%s.%s.%s" producer opName v
        let requestId = defaultArg settings.Id (generateNonce())
        let request = initRequest context.Address
        (   use stream = request.GetRequestStream() in
            use writer = XmlWriter.Create(stream)
            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnvelope)
            writer.WriteAttributeString("xmlns", "svc", null, opNamespace)

            writer.WriteStartElement("Header", XmlNamespace.SoapEnvelope)
            hf(writer, settings, producer, serviceName, requestId)
            writer.WriteEndElement()

            writer.WriteStartElement("Body", XmlNamespace.SoapEnvelope)
            writeBody(writer)
            writer.WriteEndElement()

            writer.WriteEndElement()
            writer.WriteEndDocument())
        use resp = request.GetResponse()
        use reader = new System.IO.StreamReader(resp.GetResponseStream())
        printfn "%A" (reader.ReadToEnd())
        obj()

    let rpcHeaders = [ "asutus"; "andmekogu"; "isikukood"; "ametnik"; "id"; "nimi"; "toimik"
                       "allasutus"; "amet"; "ametniknimi"; "asynkroonne"; "autentija"; "makstud"
                       "salastada"; "salastada_sertifikaadiga" ]

    let makeRpcCall (context: IXRoadContext, opnm, opver, opns, args, xthdr, writeBody) =
        let writeHeader (writer: XmlWriter, settings: XRoadHeader, producer, serviceName, requestId) =
            let writeXRoadHeader' = writeXRoadHeader XmlNamespace.Xtee writer xthdr
            writer.WriteAttributeString("xmlns", "xtee", null, XmlNamespace.Xtee)
            writeXRoadHeader' "asutus" settings.Consumer writer.WriteString
            writeXRoadHeader' "andmekogu" (Some producer) writer.WriteString
            writeXRoadHeader' "isikukood" settings.UserId writer.WriteString
            //writeXRoadHeader' "ametnik" settings.UserId writer.WriteString
            writeXRoadHeader' "id" (Some requestId) writer.WriteString
            writeXRoadHeader' "nimi" (Some serviceName) writer.WriteString
            writeXRoadHeader' "toimik" settings.Issue writer.WriteString
            writeXRoadHeader' "allasutus" settings.Unit writer.WriteString
            writeXRoadHeader' "amet" settings.Position writer.WriteString
            writeXRoadHeader' "ametniknimi" settings.UserName writer.WriteString
            writeXRoadHeader XmlNamespace.Xtee writer xthdr "asynkroonne" settings.Async writer.WriteValue
            writeXRoadHeader' "autentija" settings.Authenticator writer.WriteString
            writeXRoadHeader' "makstud" settings.Paid writer.WriteString
            writeXRoadHeader' "salastada" settings.Encrypt writer.WriteString
            writeXRoadHeader' "salastada_sertifikaadiga" settings.EncryptCert writer.WriteString
        let writeBody' (writer: XmlWriter) =
            writer.WriteStartElement(opnm, opns)
            writeBody(writer)
            writer.WriteEndElement()
        makeXRoadCall context (opnm, opver, opns) args writeHeader writeBody'

    let docHeaders = [ "consumer"; "producer"; "userId"; "id"; "service"; "issue"; "unit"; "position"
                       "userName"; "async"; "authenticator"; "paid"; "encrypt"; "encryptCert" ]

    let makeDocumentCall (context: IXRoadContext, opnm, opver, opns, args, xthdr) =
        let writeHeader (writer: XmlWriter, settings: XRoadHeader, producer, serviceName, requestId) =
            let writeXRoadHeader' = writeXRoadHeader XmlNamespace.XRoad writer xthdr
            writer.WriteAttributeString("xmlns", "xrd", null, XmlNamespace.XRoad)
            writeXRoadHeader' "consumer" settings.Consumer writer.WriteString
            writeXRoadHeader' "producer" (Some producer) writer.WriteString
            writeXRoadHeader' "userId" settings.UserId writer.WriteString
            writeXRoadHeader' "id" (Some requestId) writer.WriteString
            writeXRoadHeader' "service" (Some serviceName) writer.WriteString
            writeXRoadHeader' "issue" settings.Issue writer.WriteString
            writeXRoadHeader' "unit" settings.Unit writer.WriteString
            writeXRoadHeader' "position" settings.Position writer.WriteString
            writeXRoadHeader' "userName" settings.UserName writer.WriteString
            writeXRoadHeader XmlNamespace.Xtee writer xthdr "async" settings.Async writer.WriteValue
            writeXRoadHeader' "authenticator" settings.Authenticator writer.WriteString
            writeXRoadHeader' "paid" settings.Paid writer.WriteString
            writeXRoadHeader' "encrypt" settings.Encrypt writer.WriteString
            writeXRoadHeader' "encryptCert" settings.EncryptCert writer.WriteString
        let writeBody (writer: XmlWriter) =
            ()
        makeXRoadCall context (opnm, opver, opns) args writeHeader writeBody
