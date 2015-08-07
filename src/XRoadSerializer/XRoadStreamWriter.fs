namespace XRoad

open System.Net
open System
open System.Xml
open System.Collections.Generic
open System.IO
open System.Security.Cryptography

type ChunkState = BufferLimit | Marker | EndOfStream

type XRoadStreamWriter() =
    class
    end

type XRoadStreamReader() =
    class
    end

type XRoadProtocol =
    | Version20
    | Version30
    | Version31
    | Version40

type XRoadOptions(uri: string) =
    member val IsEncoded = false with get, set
    member val Protocol = XRoadProtocol.Version31 with get, set
    member val Uri = uri with get, set
    member val RequiredHeaders = [||] with get, set

type XRoadMessage() =
    member val Header: (XmlQualifiedName * obj) array = [||] with get, set
    member val Body: (XmlQualifiedName * obj) array = [||] with get, set
    member val Attachments = Dictionary<string, Stream>() with get, set

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
            match bytesRead with
            | 1000 -> writeChunk()
            | _ -> ()
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

    member __.SendMessage(_: XRoadMessage) =
        ()
    member __.GetResponse() =
        new XRoadResponse(request.GetResponse())

type public XRoadUtil =
    static member MakeServiceCall(message, options) =
        let request = new XRoadRequest(options)
        request.SendMessage(message)
        use response = request.GetResponse()
        response.RetrieveMessage()
