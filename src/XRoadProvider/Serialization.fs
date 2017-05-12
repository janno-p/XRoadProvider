namespace XRoad

open Common.Logging
open FSharp.Core
open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Xml
open XRoad.Serialization.Attributes
open XRoad.DynamicMethods

type XRoadFault(faultCode: string, faultString) =
    inherit Exception(faultString)
    member val FaultCode = faultCode with get
    member val FaultString = faultString with get

module Stream =
    let toString (stream: Stream) =
        stream.Position <- 0L
        let reader = new StreamReader(stream)
        reader.ReadToEnd()

type Serializer(isEncoded) as this =
    let rec skipRoot (depth: int) (reader: XmlReader) =
        if reader.Read() && reader.Depth > depth then
            if reader.NodeType = XmlNodeType.Element && reader.Depth = (depth + 1) then true
            else skipRoot depth reader
        else false

    member __.Deserialize<'T>(reader, context) : 'T =
        this.DeserializeObject(reader, typeof<'T>, context) |> Option.fold (fun _ x -> unbox x) Unchecked.defaultof<'T>

    member __.Deserialize(reader, typ, context) =
        this.DeserializeObject(reader, typ, context) |> Option.fold (fun _ x -> x) null

    member __.Serialize<'T>(writer: XmlWriter, value: 'T, rootName: XmlQualifiedName, context) =
        this.Serialize(writer, typeof<'T>, value, rootName, context)

    member __.Serialize(writer: XmlWriter, value: obj, context: SerializerContext) =
        this.SerializeObject(writer, value.GetType(), value, context)

    member __.Serialize(writer: XmlWriter, typ, value: obj, rootName: XmlQualifiedName, context) =
        match rootName.Namespace with
        | null | "" -> writer.WriteStartElement(rootName.Name)
        | _ -> writer.WriteStartElement(rootName.Name, rootName.Namespace)
        this.SerializeObject(writer, typ, value, context)
        writer.WriteEndElement()

    member private __.DeserializeObject(reader: XmlReader, typ, context) =
        match reader.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> Option.map (fun x -> x.ToLower()) with
        | Some("true") | Some("1") -> None
        | _ ->
            let typeMap = typ |> getTypeMap isEncoded
            if typeMap.Layout = Some(LayoutKind.Choice) && not (skipRoot reader.Depth reader) then None
            else Some(typeMap.Deserialize(reader, context))

    member private __.SerializeObject(writer: XmlWriter, typ, value, context) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ -> (typ |> getTypeMap isEncoded).Serialize(writer, value, context) |> ignore

module private Response =
    type XmlReader with
        member this.MoveToElement(depth, name, ns) =
            let isElement () = this.Depth = depth && (name |> isNull || (this.LocalName = name && this.NamespaceURI = ns))
            let rec findElement () =
                if isElement() then true
                elif this.Read() then
                    if this.Depth < depth then false
                    else findElement()
                else false
            isElement() || findElement()

open Response
open System.Xml.XPath

type XRoadResponse(response: WebResponse, options: XRoadResponseOptions) =
    let log = LogManager.GetLogger()

    let checkXRoadFault (stream: Stream) =
        let faultPath = "/*[local-name()='Envelope' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[local-name()='Body' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*"
        let xpath =
            match options.Protocol with
            | XRoadProtocol.Version40 -> faultPath
            | XRoadProtocol.Version20 -> sprintf "%s/keha" faultPath
            | _ -> sprintf "%s/response" faultPath
        stream.Position <- 0L
        use reader = XmlReader.Create(stream)
        let doc = XPathDocument(reader)
        let nav = doc.CreateNavigator()
        match nav.SelectSingleNode(sprintf "%s[faultCode|faultString]" xpath) with
        | null -> ()
        | node ->
            let faultCode = node.SelectSingleNode("./faultCode")
            let faultString = node.SelectSingleNode("./faultString")
            let nodeToString = Option.ofObj >> Option.map (fun x -> (x: XPathNavigator).InnerXml) >> Option.orDefault ""
            raise(XRoadFault(faultCode |> nodeToString, faultString |> nodeToString))

    member __.RetrieveMessage(): XRoadMessage =
        let message = XRoadMessage()
        use stream =
            let stream, attachments = response |> MultipartMessage.read
            attachments |> List.iter (fun content -> message.Attachments.Add(content.ContentID, content))
            stream
        if log.IsTraceEnabled then
            log.Trace(stream |> Stream.toString)
        stream |> checkXRoadFault
        stream.Position <- 0L
        use reader = XmlReader.Create(stream)
        if not (reader.MoveToElement(0, "Envelope", XmlNamespace.SoapEnv)) then
            failwith "Soap envelope element was not found in response message."
        if not (reader.MoveToElement(1, "Body", XmlNamespace.SoapEnv)) then
            failwith "Soap body element was not found in response message."
        let context = SerializerContext()
        context.AddAttachments(message.Attachments)
        let serializer = Serializer(options.IsEncoded)
        if not (reader.MoveToElement(2, null, null)) then
            failwith "Soap message has empty payload in response."
        message.Body <-
            match reader.LocalName, reader.NamespaceURI with
            | "Fault", XmlNamespace.SoapEnv -> failwithf "Request resulted an error: %s" (reader.ReadInnerXml())
            | _ -> serializer.Deserialize(reader, options.ResponseType, context)
        message

    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()

type XRoadStreamWriter() =
    class
    end

type XRoadStreamReader() =
    class
    end

type XRoadRequest(opt: XRoadRequestOptions) =
    let log = LogManager.GetLogger()

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

    let writeIdHeader value ns req (writer: XmlWriter) =
        if req |> Array.exists ((=) "id") || value |> isNullOrEmpty |> not then
            writer.WriteStartElement("id", ns)
            if opt.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("string", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            writer.WriteValue(if String.IsNullOrWhiteSpace(value) then XRoadHelper.getUUID() else value)
            writer.WriteEndElement()

    let writeStringHeader req ns (writer: XmlWriter) value name =
        if req |> Array.exists ((=) name) || value |> isNullOrEmpty |> not then
            writer.WriteStartElement(name, ns)
            if opt.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("string", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if String.IsNullOrEmpty(value) |> not then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let writeBoolHeader (value: Nullable<bool>) name ns req (writer: XmlWriter) =
        if req |> Array.exists ((=) name) || value.HasValue then
            writer.WriteStartElement(name, ns)
            if opt.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("boolean", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if value.HasValue then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let writeBase64Header (value: byte[]) name ns req (writer: XmlWriter) =
        if req |> Array.exists ((=) name) || value |> isNullOrEmptyArray |> not then
            writer.WriteStartElement(name, ns)
            if opt.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("base64", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if value |> isNull |> not && value |> Array.isEmpty |> not then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let writeClientHeader (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
        if req |> Array.exists ((=) "client") || value |> isNotNull then
            writer.WriteStartElement("client", XmlNamespace.XRoad40)
            if value |> isNotNull then
                writer.WriteStartAttribute("objectType", XmlNamespace.XRoad40Id)
                writer.WriteValue(if String.IsNullOrWhiteSpace(value.SubsystemCode) then "MEMBER" else "SUBSYSTEM")
                writer.WriteEndAttribute()
                writer.WriteStartElement("xRoadInstance", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.XRoadInstance) then
                    writer.WriteValue(value.XRoadInstance)
                writer.WriteEndElement()
                writer.WriteStartElement("memberClass", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberClass) then
                    writer.WriteValue(value.MemberClass)
                writer.WriteEndElement()
                writer.WriteStartElement("memberCode", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberCode) then
                    writer.WriteValue(value.MemberCode)
                writer.WriteEndElement()
                if String.IsNullOrWhiteSpace(value.SubsystemCode) |> not then
                    writer.WriteStartElement("subsystemCode", XmlNamespace.XRoad40Id)
                    writer.WriteValue(value.SubsystemCode)
                    writer.WriteEndElement()
            writer.WriteEndElement()

    let getServiceName producerName =
        let serviceName = if opt.ServiceVersion |> isNullOrEmpty then opt.ServiceCode
                          else sprintf "%s.%s" opt.ServiceCode opt.ServiceVersion
        if producerName |> isNullOrEmpty then serviceName
        else sprintf "%s.%s" producerName serviceName

    let writeServiceHeader (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
        if req |> Array.exists ((=) "service") || value |> isNotNull then
            writer.WriteStartElement("service", XmlNamespace.XRoad40)
            if value |> isNotNull then
                writer.WriteStartAttribute("objectType", XmlNamespace.XRoad40Id)
                writer.WriteValue("SERVICE")
                writer.WriteEndAttribute()
                writer.WriteStartElement("xRoadInstance", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.XRoadInstance) then
                    writer.WriteValue(value.XRoadInstance)
                writer.WriteEndElement()
                writer.WriteStartElement("memberClass", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberClass) then
                    writer.WriteValue(value.MemberClass)
                writer.WriteEndElement()
                writer.WriteStartElement("memberCode", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberCode) then
                    writer.WriteValue(value.MemberCode)
                writer.WriteEndElement()
                if String.IsNullOrWhiteSpace(value.SubsystemCode) |> not then
                    writer.WriteStartElement("subsystemCode", XmlNamespace.XRoad40Id)
                    writer.WriteValue(value.SubsystemCode)
                    writer.WriteEndElement()
                writer.WriteStartElement("serviceCode", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(opt.ServiceCode) then
                    writer.WriteValue(opt.ServiceCode)
                writer.WriteEndElement()
                if String.IsNullOrWhiteSpace(opt.ServiceVersion) |> not then
                    writer.WriteStartElement("serviceVersion", XmlNamespace.XRoad40Id)
                    writer.WriteValue(opt.ServiceVersion)
                    writer.WriteEndElement()
            writer.WriteEndElement()

    let writeXRoadHeader (msg: XRoadMessage) (writer: XmlWriter) =
        if writer.LookupPrefix(msg.HeaderNamespace) |> isNull then
            writer.WriteAttributeString("xmlns", protocolPrefix opt.Protocol, XmlNamespace.Xmlns, msg.HeaderNamespace)
        let writeStringHeader' = writeStringHeader msg.RequiredHeaders msg.HeaderNamespace writer
        match msg.Header with
        | :? XRoadRpcHeader as header ->
            writeStringHeader' header.Asutus "asutus"
            writeStringHeader' header.Andmekogu "andmekogu"
            writeStringHeader' header.Isikukood "isikukood"
            writeStringHeader' header.Ametnik "ametnik"
            writer |> writeIdHeader header.Id msg.HeaderNamespace msg.RequiredHeaders
            writeStringHeader' (getServiceName header.Andmekogu) "nimi"
            writeStringHeader' header.Toimik "toimik"
            writeStringHeader' header.Allasutus "allasutus"
            writeStringHeader' header.Amet "amet"
            writeStringHeader' header.AmetnikNimi "ametniknimi"
            writer |> writeBoolHeader header.Asynkroonne "asynkroonne" msg.HeaderNamespace msg.RequiredHeaders
            writeStringHeader' header.Autentija "autentija"
            writeStringHeader' header.Makstud "makstud"
            writeStringHeader' header.Salastada "salastada"
            writer |> writeBase64Header header.SalastadaSertifikaadiga "salastada_sertifikaadiga" msg.HeaderNamespace msg.RequiredHeaders
            writeStringHeader' header.Salastatud "salastatud"
            writeStringHeader' header.SalastatudSertifikaadiga "salastatud_sertifikaadiga"
        | :? XRoadDocHeader as header ->
            writeStringHeader' header.Consumer "consumer"
            writeStringHeader' header.Producer "producer"
            writeStringHeader' header.UserId "userId"
            writer |> writeIdHeader header.Id msg.HeaderNamespace msg.RequiredHeaders
            writeStringHeader' (getServiceName header.Producer) "service"
            writeStringHeader' header.Issue "issue"
            writeStringHeader' header.Unit "unit"
            writeStringHeader' header.Position "position"
            writeStringHeader' header.UserName "userName"
            writer |> writeBoolHeader header.Async "async" msg.HeaderNamespace msg.RequiredHeaders
            writeStringHeader' header.Authenticator "authenticator"
            writeStringHeader' header.Paid "paid"
            writeStringHeader' header.Encrypt "encrypt"
            writer |> writeBase64Header header.EncryptCert "encryptCert" msg.HeaderNamespace msg.RequiredHeaders
            writeStringHeader' header.Encrypted "encrypted"
            writeStringHeader' header.EncryptedCert "encryptedCert"
        | :? XRoadHeader as header ->
            if writer.LookupPrefix(XmlNamespace.XRoad40Id) |> isNull then
                writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoad40Id)
            writer |> writeClientHeader header.Client msg.RequiredHeaders
            writer |> writeServiceHeader header.Producer msg.RequiredHeaders
            writer |> writeIdHeader header.Id msg.HeaderNamespace msg.RequiredHeaders
            writeStringHeader' header.UserId "userId"
            writeStringHeader' header.Issue "issue"
            writeStringHeader' header.ProtocolVersion "protocolVersion"
        | _ -> failwithf "Unexpected X-Road header type `%s`." (msg.Header.GetType().FullName)
        msg.Header.Unresolved |> Seq.iter (fun e -> e.WriteTo(writer))

    member __.SendMessage(msg: XRoadMessage) =
        use content = new MemoryStream()
        use sw = new StreamWriter(content)
        let context = SerializerContext(IsMultipart = opt.IsMultipart)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", protocolPrefix opt.Protocol, XmlNamespace.Xmlns, protocolNamespace opt.Protocol)
        msg.Namespaces |> Seq.iteri (fun i ns -> writer.WriteAttributeString("xmlns", sprintf "ns%d" i, XmlNamespace.Xmlns, ns))
        match opt.Accessor with | null -> () | acc -> writer.WriteAttributeString("xmlns", "acc", XmlNamespace.Xmlns, acc.Namespace)
        if opt.IsEncoded then
            writer.WriteAttributeString("xmlns", "xsd", XmlNamespace.Xmlns, XmlNamespace.Xsd)
            writer.WriteAttributeString("encodingStyle", XmlNamespace.SoapEnv, XmlNamespace.SoapEnc)
        writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
        writer |> writeXRoadHeader msg
        writer.WriteEndElement()

        let serializeAccessor funContent =
            match opt.Accessor with
            | null -> funContent()
            | _ -> writer.WriteStartElement(opt.Accessor.Name, opt.Accessor.Namespace)
                   funContent()
                   writer.WriteEndElement()

        writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
        serializeAccessor (fun _ -> Serializer(opt.IsEncoded).Serialize(writer, msg.Body, context))
        writer.WriteEndElement()

        writer.WriteEndDocument()
        writer.Flush()
        if log.IsTraceEnabled then
            log.Trace(content |> Stream.toString)
        serializeMessage content context.Attachments
    member __.GetResponse(options) =
        new XRoadResponse(request.GetResponse(), options)

type public XRoadUtil =
    static member MakeServiceCall(message, requestOptions, responseOptions) =
        let request = XRoadRequest(requestOptions)
        request.SendMessage(message)
        use response = request.GetResponse(responseOptions)
        response.RetrieveMessage()
