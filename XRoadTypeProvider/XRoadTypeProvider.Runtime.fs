module XRoadTypeProvider.Runtime

open System
open System.Collections.Generic
open System.IO
open System.Xml
open XRoadTypeProvider.Wsdl

type XRoadEntity () =
    let data = Dictionary<string, obj>()

    member __.SetProperty (name, value) =
        data.[name] <- box value

    member __.GetProperty<'T> (name) =
        if data.ContainsKey name then
            unbox data.[name]
        else Unchecked.defaultof<'T>

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

type XRoadServiceRequest () =
    let generateNonce () =
        ""

    member __.Execute(context: IXRoadContext, operationName, args: obj []) =
        let settings = args |> Array.tryFind (fun arg -> arg :? XRoadHeader)
                            |> Option.map (fun o -> o :?> XRoadHeader)
                            |> Option.orDefault (XRoadHeader())

        let req = System.Net.WebRequest.Create(context.Address)
        req.Method <- "POST"

        let producer = defaultArg settings.Producer context.Producer
        let serviceName = defaultArg settings.Service (sprintf "%s.%s" producer operationName)
        let requestId = defaultArg settings.Id (generateNonce())

        let writeReq () =
            use stream = req.GetRequestStream()
            use writer = XmlWriter.Create(stream)

            let writerXRoadProperty name value =
                writer.WriteStartElement(name, XmlNamespace.XRoad)
                match value with
                | Some value -> writer.WriteString(value)
                | _ -> ()
                writer.WriteEndElement()

            let writeOptionalXRoadProperty name (value: 'T option) (f: 'T -> unit) =
                match value with
                | Some value ->
                    writer.WriteStartElement(name, XmlNamespace.XRoad)
                    f(value)
                    writer.WriteEndElement()
                | _ -> ()

            let writeSoapHeader () =
                writer.WriteStartElement("Header", XmlNamespace.SoapEnvelope)
                writer.WriteAttributeString("xmlns", "xrd", null, XmlNamespace.XRoad)

                // TODO: Required elements are declared in WSDL. Others are optional.
                writerXRoadProperty "consumer" settings.Consumer
                writerXRoadProperty "producer" (Some producer)
                writerXRoadProperty "userId" settings.UserId
                writerXRoadProperty "id" (Some requestId)
                writerXRoadProperty "service" (Some serviceName)
                writerXRoadProperty "issue" None

                writeOptionalXRoadProperty "unit" settings.Unit writer.WriteString
                writeOptionalXRoadProperty "position" settings.Position writer.WriteString
                writeOptionalXRoadProperty "userName" settings.UserName writer.WriteString
                writeOptionalXRoadProperty "async" settings.Async writer.WriteValue
                writeOptionalXRoadProperty "authenticator" settings.Authenticator writer.WriteString
                writeOptionalXRoadProperty "paid" settings.Paid writer.WriteString
                writeOptionalXRoadProperty "encrypt" settings.Encrypt writer.WriteString
                writeOptionalXRoadProperty "encryptCert" settings.EncryptCert writer.WriteString

                writer.WriteEndElement()

            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnvelope)
            writeSoapHeader()
            writer.WriteStartElement("Body", XmlNamespace.SoapEnvelope)

            writer.WriteStartElement("listMethods")
            writer.WriteEndElement()

            writer.WriteEndElement()
            writer.WriteEndElement()
            writer.WriteEndDocument()

        writeReq()

        use resp = req.GetResponse()
        use reader = new System.IO.StreamReader(resp.GetResponseStream())
        printfn "%A" (reader.ReadToEnd())
        obj()
    interface IDisposable with
        override __.Dispose() = ()

