namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Serialization
open System.Security.Cryptography

type IXmlBookmarkReader =
    abstract Reader: XmlReader with get

type public XRoadSerializerContext() =
    let attachments = Dictionary<string, Stream>()
    member val IsMultipart = false with get, set
    member val Attachments = attachments with get
    member this.AddAttachments(attachments: IDictionary<_,_>) =
        match attachments with
        | null -> ()
        | _ -> attachments |> Seq.iter (fun kvp -> this.Attachments.Add(kvp.Key, kvp.Value))

type public XRoadXmlReader(stream, context: XRoadSerializerContext) =
    inherit XmlTextReader(stream: Stream)
    member val Context = context with get

type public XRoadXmlWriter(writer, context: XRoadSerializerContext) =
    inherit XmlTextWriter(writer)
    member val Context = context with get

type public BinaryContent(contentID: string, content: Stream) as this =
    let [<Literal>] BufferSize = 1000
    new() = BinaryContent(null, null)
    new(content) = BinaryContent(null, content)
    member val ContentID = contentID with get, set
    member val Content = content with get, set
    interface IXmlSerializable with
        member __.GetSchema() = null
        member __.ReadXml(reader) =
            let contextProvider =
                let r =
                    match box reader with
                    | :? IXmlBookmarkReader as xbr -> xbr.Reader
                    | _ -> reader
                r :?> XRoadXmlReader
            let context = contextProvider.Context
            let isEmpty = reader.IsEmptyElement
            let contentID = reader.GetAttribute("href")
            reader.ReadStartElement()
            if context.IsMultipart && not(String.IsNullOrWhiteSpace(contentID)) then
                this.ContentID <- match contentID.StartsWith("cid:") with true -> contentID.Substring(4) | _ -> contentID
                this.Content <- context.Attachments.[this.ContentID]
            if not isEmpty then
                let content = new MemoryStream()
                let buffer = Array.create BufferSize 0uy
                let rec readContent () =
                    let bytesRead = reader.ReadContentAsBase64(buffer, 0, BufferSize)
                    content.Write(buffer, 0, bytesRead)
                    if BufferSize <= bytesRead then readContent()
                readContent()
                if String.IsNullOrWhiteSpace(this.ContentID) then
                    content.Position <- 0L
                    this.ContentID <- Convert.ToBase64String(SHA1.Create().ComputeHash(content))
                    content.Position <- 0L
                    this.Content <- content
                reader.ReadEndElement()
        member __.WriteXml(w) =
            match w with
            | :? XRoadXmlWriter as writer ->
                match this.Content with
                | null -> writer.WriteAttributeString("nil", "http://www.w3.org/2001/XMLSchema-instance", "true")
                | content ->
                    content.Position <- 0L
                    match writer.Context.IsMultipart with
                    | true ->
                        let contentIDBase = match this.ContentID with null -> Convert.ToBase64String(SHA1.Create().ComputeHash(content)) | s -> s
                        let rec getUniqueContentID contentID counter =
                            if writer.Context.Attachments.ContainsKey(contentID) then
                                let counter = counter + 1
                                getUniqueContentID (sprintf "%s%d" contentIDBase counter) counter
                            else contentID
                        let contentID = getUniqueContentID contentIDBase 0
                        writer.WriteAttributeString("href", sprintf "cid:%s" contentID)
                        writer.Context.Attachments.Add(contentID, content)
                    | false ->
                        let buffer = Array.create BufferSize 0uy
                        let rec writeContent () =
                            let bytesRead = content.Read(buffer, 0, BufferSize)
                            writer.WriteBase64(buffer, 0, bytesRead)
                            if BufferSize <= bytesRead then writeContent()
                        writeContent()
            | _ -> failwith "Not implemented."
