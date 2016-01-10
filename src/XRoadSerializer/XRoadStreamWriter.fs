namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Xml

type XRoadStreamWriter() =
    class
    end

type XRoadStreamReader() =
    class
    end

type XRoadRequest(opt: XRoadRequestOptions) =
    let request = WebRequest.Create(opt.Uri, Method="POST", ContentType="text/xml; charset=utf-8")
    do request.Headers.Set("SOAPAction", "")

    let writeContent (stream: Stream) (content: Stream) =
        let buffer = Array.create 1000 0uy
        let rec writeChunk() =
            let bytesRead = content.Read(buffer, 0, 1000)
            stream.Write(buffer, 0, bytesRead)
            match bytesRead with 1000 -> writeChunk() | _ -> ()
        content.Position <- 0L
        writeChunk()

    let serializeMultipartMessage (attachments: Dictionary<string,BinaryContent>) (serializeContent: Stream -> unit) =
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
                use contentStream = kvp.Value.OpenStream()
                writeContent stream contentStream
                writer.WriteLine())
            writer.WriteLine("--{0}--", boundaryMarker)
        else stream |> serializeContent

    let serializeMessage (content: Stream) (attachments: Dictionary<string,BinaryContent>) =
        serializeMultipartMessage attachments (fun s -> writeContent s content)

    let (|XteeName|_|) (name: XmlQualifiedName) =
        match name.Namespace with
        | XmlNamespace.Xtee -> Some name.Name
        | _ -> None

    let getHeaderElementType = function
        | XteeName "asutus"
        | XteeName "andmekogu"
        | XteeName "isikukood"
        | XteeName "ametnik"
        | XteeName "id"
        | XteeName "nimi"
        | XteeName "toimik"
        | XteeName "allasutus"
        | XteeName "amet"
        | XteeName "autentija"
        | XteeName "makstud"
        | XteeName "salastada"
        | XteeName "salastatud"
        | XteeName "salastatud_sertifikaadiga"
        | XteeName "ametniknimi" -> Some(XmlQualifiedName("string", XmlNamespace.Xsd))
        | XteeName "asynkroonne" -> Some(XmlQualifiedName("boolean", XmlNamespace.Xsd))
        | XteeName "salastada_sertifikaadiga" -> Some(XmlQualifiedName("base64", XmlNamespace.Xsd))
        | _ -> None

    member __.SendMessage(msg: XRoadMessage) =
        use content = new MemoryStream()
        use sw = new StreamWriter(content)
        let context = SerializerContext(IsMultipart = opt.IsMultipart)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", protocolPrefix opt.Protocol, XmlNamespace.Xmlns, protocolNamespace opt.Protocol)
        msg.Body
        |> Array.map (fun (nm,_) -> nm.Namespace)
        |> Seq.filter (fun ns -> not <| String.IsNullOrWhiteSpace(ns))
        |> Seq.iteri (fun i ns -> writer.WriteAttributeString("xmlns", sprintf "ns%d" i, XmlNamespace.Xmlns, ns))
        match opt.Accessor with | null -> () | acc -> writer.WriteAttributeString("xmlns", "acc", XmlNamespace.Xmlns, acc.Namespace)
        if opt.IsEncoded then
            writer.WriteAttributeString("xmlns", "xsd", XmlNamespace.Xmlns, XmlNamespace.Xsd)
            writer.WriteAttributeString("encodingStyle", XmlNamespace.SoapEnv, XmlNamespace.SoapEnc)
        writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
        msg.Header
        |> Array.iter (fun hdr ->
            if hdr.IsRequired || hdr.Value <> null then
                writer.WriteStartElement(hdr.Name.Name, hdr.Name.Namespace)
                if opt.IsEncoded then
                    hdr.Name
                    |> getHeaderElementType
                    |> Option.iter (fun nm ->
                        writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                        writer.WriteQualifiedName(nm.Name, nm.Namespace)
                        writer.WriteEndAttribute())
                if not (hdr.Value |> isNull) then
                    writer.WriteValue(hdr.Value)
                elif hdr.Name.Name = "id" && (hdr.Name.Namespace = XmlNamespace.XRoad || hdr.Name.Namespace = XmlNamespace.Xtee) then
                    writer.WriteValue(XRoadHelper.generateNonce())
                writer.WriteEndElement())
        writer.WriteEndElement()

        let serializeBody funContent =
            match msg.Body with
            | [| (null, _) |] -> funContent()
            | _ -> writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
                   funContent()
                   writer.WriteEndElement()

        let serializeAccessor funContent =
            match opt.Accessor with
            | null -> funContent()
            | _ -> writer.WriteStartElement(opt.Accessor.Name, opt.Accessor.Namespace)
                   funContent()
                   writer.WriteEndElement()

        serializeBody (fun _ ->
            serializeAccessor (fun _ ->
                msg.Body
                |> Array.iteri (fun i (name,value) ->
                    Serializer(opt.IsEncoded).Serialize(writer, opt.Types.[i], value, (if isNull name then XmlQualifiedName("Body", XmlNamespace.SoapEnv) else name), context))))

        writer.WriteEndDocument()
        writer.Flush()
        serializeMessage content context.Attachments
    member __.GetResponse(options) =
        new XRoadResponse(request.GetResponse(), options)

type public XRoadUtil =
    static member MakeServiceCall(message, requestOptions, responseOptions) =
        let request = new XRoadRequest(requestOptions)
        request.SendMessage(message)
        use response = request.GetResponse(responseOptions)
        response.RetrieveMessage()
