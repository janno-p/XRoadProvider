namespace XRoad

open System.Collections.Generic
open System.IO
open System.Xml

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
