namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Security.Cryptography
open System.Xml

type ChunkState = BufferLimit | Marker | EndOfStream

type SoapHeaderValue(name: XmlQualifiedName, value: obj, required: bool) =
    member val Name = name with get
    member val Value = value with get
    member val IsRequired = required with get

type XRoadStreamWriter() =
    class
    end

type XRoadStreamReader() =
    class
    end

type XRoadProtocol =
    | Version20 = 0
    | Version30 = 1
    | Version31 = 2
    | Version40 = 3

[<AutoOpen>]
module private XRoadProtocolExtensions =
    let protocolPrefix = function
        | XRoadProtocol.Version20 -> "xtee"
        | XRoadProtocol.Version30
        | XRoadProtocol.Version31 -> "xrd"
        | XRoadProtocol.Version40 -> failwith "Not implemented v4.0"
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

    let protocolNamespace = function
        | XRoadProtocol.Version20 -> XmlNamespace.Xtee
        | XRoadProtocol.Version30 -> XmlNamespace.Xrd
        | XRoadProtocol.Version31 -> XmlNamespace.XRoad
        | XRoadProtocol.Version40 -> failwith "Not implemented v4.0"
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

type XRoadOptions(uri: string, isEncoded: bool, isMultipart: bool, protocol: XRoadProtocol) =
    member val IsEncoded = isEncoded with get
    member val IsMultipart = isMultipart with get
    member val Protocol = protocol with get, set
    member val Uri = uri with get, set

type XRoadMessage() =
    member val Header: SoapHeaderValue array = [||] with get, set
    member val Body: (XmlQualifiedName * obj) array = [||] with get, set
    member val Attachments = Dictionary<string, Stream>() with get, set
    member val Accessor: XmlQualifiedName = null with get, set
    member this.GetPart(name) =
        this.Body
        |> Array.tryFind (fst >> ((=) name))
        |> Option.map (snd)
        |> Option.fold (fun _ x -> x) null

type XRoadResponse(response: WebResponse) =
    member __.RetrieveMessage(): XRoadMessage =
        XRoadMessage()
    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()

type XRoadRequest(opt: XRoadOptions) =
    let request = WebRequest.Create(opt.Uri, Method="POST", ContentType="text/xml; charset=utf-8")
    do request.Headers.Set("SOAPAction", "")

    let generateNonce() =
        let nonce = Array.create 42 0uy
        RNGCryptoServiceProvider.Create().GetNonZeroBytes(nonce)
        Convert.ToBase64String(nonce)

    let writeContent (stream: Stream) (content: Stream) =
        let buffer = Array.create 1000 0uy
        let rec writeChunk() =
            let bytesRead = content.Read(buffer, 0, 1000)
            stream.Write(buffer, 0, bytesRead)
            match bytesRead with 1000 -> writeChunk() | _ -> ()
        content.Position <- 0L
        writeChunk()

    let serializeMultipartMessage (attachments: IDictionary<string, Stream>) (serializeContent: Stream -> unit) =
        use stream = request.GetRequestStream()
        if attachments.Count > 0 then
            use writer = new StreamWriter(stream, NewLine = "\r\n")
            let boundaryMarker = Guid.NewGuid().ToString()
            request.ContentType <- sprintf @"multipart/related; type=""text/xml""; boundary=""%s""" boundaryMarker
            request.Headers.Add("MIME-Version", "1.0")
            writer.WriteLine()
            writer.WriteLine("--{0}", boundaryMarker)
            writer.WriteLine("Content-Type: text/xml; charset=UTF-8")
            writer.WriteLine("Content-Transfer-Encoding: 8bit")
            writer.WriteLine("Content-ID: <XML-{0}>", boundaryMarker)
            writer.WriteLine()
            writer.Flush()
            stream |> serializeContent
            attachments |> Seq.iter (fun kvp ->
                writer.WriteLine()
                writer.WriteLine("--{0}", boundaryMarker)
                writer.WriteLine("Content-Disposition: attachment; filename=notAnswering")
                writer.WriteLine("Content-Type: application/octet-stream")
                writer.WriteLine("Content-Transfer-Encoding: binary")
                writer.WriteLine("Content-ID: <{0}>", kvp.Key)
                writer.WriteLine()
                writer.Flush()
                writeContent stream kvp.Value
                writer.WriteLine())
            writer.WriteLine("--{0}--", boundaryMarker)
        else stream |> serializeContent

    let serializeMessage (content: Stream) (attachments: IDictionary<string, Stream>) =
        serializeMultipartMessage attachments (fun s -> writeContent s content)

    member __.SendMessage(msg: XRoadMessage) =
        use content = new MemoryStream()
        use sw = new StreamWriter(content)
        use writer = new XRoadXmlWriter(sw, XRoadSerializerContext(IsMultipart = opt.IsMultipart))
        writer.WriteStartDocument()
        writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", protocolPrefix opt.Protocol, XmlNamespace.Xmlns, protocolNamespace opt.Protocol)
        match msg.Accessor with | null -> () | acc -> writer.WriteAttributeString("xmlns", "acc", XmlNamespace.Xmlns, acc.Namespace)
        if opt.IsEncoded then
            writer.WriteAttributeString("encodingStyle", XmlNamespace.SoapEnv, XmlNamespace.SoapEnc)
        writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
        msg.Header
        |> Array.iter (fun hdr ->
            if hdr.IsRequired || hdr.Value <> null then
                writer.WriteStartElement(hdr.Name.Name, hdr.Name.Namespace)
                if not (hdr.Value |> isNull) then
                    writer.WriteValue(hdr.Value)
                elif hdr.Name.Name = "id" && (hdr.Name.Namespace = XmlNamespace.XRoad || hdr.Name.Namespace = XmlNamespace.Xtee) then
                    writer.WriteValue(generateNonce())
                writer.WriteEndElement())
        writer.WriteEndElement()

        let serializeBody funContent =
            match msg.Body with
            | [| (null, _) |] -> funContent()
            | _ -> writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
                   funContent()
                   writer.WriteEndElement()

        let serializeAccessor funContent =
            match msg.Accessor with
            | null -> funContent()
            | _ -> writer.WriteStartElement(msg.Accessor.Name, msg.Accessor.Namespace)
                   funContent()
                   writer.WriteEndElement()

        serializeBody (fun _ ->
            serializeAccessor (fun _ ->
                msg.Body
                |> Array.iter (fun (name,value) ->
                    Serializer().Serialize(writer, value, if isNull name then XmlQualifiedName("Body", XmlNamespace.SoapEnv) else name))))

        writer.WriteEndDocument()
        writer.Flush()
        serializeMessage content (dict [])
    member __.GetResponse() =
        new XRoadResponse(request.GetResponse())

type public XRoadUtil =
    static member MakeServiceCall(message, options) =
        let request = new XRoadRequest(options)
        request.SendMessage(message)
        use response = request.GetResponse()
        response.RetrieveMessage()
