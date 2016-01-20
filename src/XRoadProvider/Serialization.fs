namespace XRoad

open FSharp.Core
open System.IO
open System.Xml
open XRoad.Serialization.Attributes
open XRoad.DynamicMethods

module Stream =
    let toString (stream: Stream) =
        stream.Position <- 0L
        use reader = new StreamReader(stream)
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

namespace XRoad

open Common.Logging
open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text
open System.Xml

module private Response =
    type ChunkState = Limit | NewLine | EndOfStream

    type PeekStream(stream: Stream) =
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

    let getBoundaryMarker (response: WebResponse) =
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

    let chunkSize = 4096
    let cr = Convert.ToInt32('\r')
    let lf = Convert.ToInt32('\n')

    let readChunkOrLine (buffer: byte []) (stream: PeekStream) =
        let rec addByte pos =
            if pos >= chunkSize then (ChunkState.Limit, pos)
            else
                match stream.Read() with
                | -1 -> (ChunkState.EndOfStream, pos)
                | byt ->
                    if byt = cr && stream.Peek() = lf then
                        stream.Read() |> ignore
                        (ChunkState.NewLine, pos)
                    else
                        buffer.[pos] <- Convert.ToByte(byt)
                        addByte (pos + 1)
        let result = addByte 0
        stream.Flush()
        result

    let readLine stream =
        let mutable line = [| |] : byte []
        let buffer = Array.zeroCreate<byte>(chunkSize)
        let rec readChunk () =
            let (state, chunkSize) = stream |> readChunkOrLine buffer
            Array.Resize(&line, line.Length + chunkSize)
            match state with
            | ChunkState.Limit -> readChunk()
            | ChunkState.EndOfStream
            | ChunkState.NewLine -> ()
        line

    let extractMultipartContentHeaders (stream: PeekStream) =
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

    let base64Decoder (encoding: Encoding) (encodedBytes: byte []) =
        match encodedBytes with
        | null | [| |] -> [| |]
        | _ ->
            let chars = encoding.GetChars(encodedBytes)
            Convert.FromBase64CharArray(chars, 0, chars.Length)

    let getDecoder (contentEncoding: string) =
        match contentEncoding.ToLower() with
        | "base64" -> Some(base64Decoder)
        | "quoted-printable" | "7bit" | "8bit" | "binary" -> None
        | _ -> failwithf "No decoder implemented for content transfer encoding `%s`." contentEncoding

    let startsWith (value: byte []) (buffer: byte []) =
        let rec compare i =
            if value.[i] = buffer.[i] then
                if i = 0 then true else compare (i - 1)
            else false
        if buffer |> isNull || value |> isNull || value.Length > buffer.Length then false
        else compare (value.Length - 1)

    let parseResponse (response: WebResponse) : Stream * BinaryContent list =
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
            let buffer = Array.zeroCreate<byte>(chunkSize)
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

    type XmlReader with
        member this.MoveToElement(depth, name, ns) =
            let isElement () = this.Depth = depth && this.LocalName = name && this.NamespaceURI = ns
            let rec findElement () =
                if isElement() then true
                elif this.Read() then
                    if this.Depth < depth then false
                    else findElement()
                else false
            isElement() || findElement()

open Response

type XRoadResponse(response: WebResponse, options: XRoadResponseOptions) =
    let log = LogManager.GetLogger()
    member __.RetrieveMessage(): XRoadMessage =
        let message = XRoadMessage()
        use stream =
            let stream, attachments = Response.parseResponse(response)
            attachments |> List.iter (fun content -> message.Attachments.Add(content.ContentID, content))
            stream
        log.Trace(fun m -> m.Invoke(stream |> Stream.toString) |> ignore)
        stream.Position <- 0L
        let reader = XmlReader.Create(stream)
        if not (reader.MoveToElement(0, "Envelope", XmlNamespace.SoapEnv)) then
            failwith "Soap envelope element was not found in response message."
        if not (reader.MoveToElement(1, "Body", XmlNamespace.SoapEnv)) then
            failwith "Soap body element was not found in response message."
        if options.ExpectUnexpected then
            // TODO: check if unexpected exception was thrown by service.
            ()
        let context = SerializerContext()
        context.AddAttachments(message.Attachments)
        let serializer = Serializer(options.IsEncoded)
        let rec findElements (parameters: List<_>) =
            if reader.Depth = 2 && reader.NodeType = XmlNodeType.Element then
                match reader.LocalName, reader.NamespaceURI with
                | "Fault", XmlNamespace.SoapEnv ->
                    failwithf "Request resulted an error: %s" (reader.ReadInnerXml())
                | nm, ns ->
                    let qualifiedName = XmlQualifiedName(nm, ns)
                    parameters.Add(qualifiedName, serializer.Deserialize(reader, options.Types.[qualifiedName], context))
            if not (reader.Read()) || reader.Depth < 2 then parameters.ToArray()
            else findElements parameters
        reader.Read() |> ignore
        message.Body <- findElements(List<_>())
        message

    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()

namespace XRoad

open Common.Logging
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

    let (|XteeName|_|) (name: XmlQualifiedName) =
        match name.Namespace with
        | XmlNamespace.XRoad20 -> Some name.Name
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
                elif hdr.Name.Name = "id" && (hdr.Name.Namespace = XmlNamespace.XRoad31Ee || hdr.Name.Namespace = XmlNamespace.XRoad20) then
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
        log.Trace(fun m -> m.Invoke(content |> Stream.toString) |> ignore)
        serializeMessage content context.Attachments
    member __.GetResponse(options) =
        new XRoadResponse(request.GetResponse(), options)

type public XRoadUtil =
    static member MakeServiceCall(message, requestOptions, responseOptions) =
        let request = new XRoadRequest(requestOptions)
        request.SendMessage(message)
        use response = request.GetResponse(responseOptions)
        response.RetrieveMessage()
