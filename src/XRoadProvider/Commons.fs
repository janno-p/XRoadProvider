namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Xml

[<RequireQualifiedAccessAttribute>]
module internal XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xop = "http://www.w3.org/2004/08/xop/include"
    let [<Literal>] XRoad20 = "http://x-tee.riik.ee/xsd/xtee.xsd"
    let [<Literal>] XRoad30 = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad31Ee = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] XRoad31Eu = "http://x-road.eu/xsd/x-road.xsd"
    let [<Literal>] XRoad40 = "http://x-road.eu/xsd/xroad.xsd"
    let [<Literal>] XRoad40Id = "http://x-road.eu/xsd/identifiers"
    let [<Literal>] XRoad40Repr = "http://xroad.eu/xsd/representation.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"

type XRoadProtocol =
    | Undefined = 0
    | Version20 = 1
    | Version30 = 2
    | Version31 = 3
    | Version40 = 4

[<AutoOpen>]
module internal Option =
    let ofObj o = match o with null -> None | x -> Some(x)

[<AutoOpen>]
module internal List =
    let tryHead lst = match lst with [] -> None | x::_ -> Some(x)

[<AutoOpen>]
module Commons =
    let isNull o = (o = null)

[<AutoOpen>]
module private XRoadProtocolExtensions =
    let protocolPrefix = function
        | XRoadProtocol.Version20 -> "xtee"
        | XRoadProtocol.Version30
        | XRoadProtocol.Version31 -> "xrd"
        | XRoadProtocol.Version40 -> failwith "Not implemented v4.0"
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

    let protocolNamespace = function
        | XRoadProtocol.Version20 -> XmlNamespace.XRoad20
        | XRoadProtocol.Version30 -> XmlNamespace.XRoad30
        | XRoadProtocol.Version31 -> XmlNamespace.XRoad31Ee
        | XRoadProtocol.Version40 -> failwith "Not implemented v4.0"
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

module XRoadHelper =
    let generateNonce() =
        let nonce = Array.create 42 0uy
        RNGCryptoServiceProvider.Create().GetNonZeroBytes(nonce)
        Convert.ToBase64String(nonce)

    let getSystemTypeName = function
        | "System.String" -> Some(XmlQualifiedName("string", XmlNamespace.Xsd))
        | "System.Boolean" -> Some(XmlQualifiedName("boolean", XmlNamespace.Xsd))
        | "System.DateTime" -> Some(XmlQualifiedName("dateTime", XmlNamespace.Xsd))
        | "System.Decimal" -> Some(XmlQualifiedName("decimal", XmlNamespace.Xsd))
        | "System.Double" -> Some(XmlQualifiedName("double", XmlNamespace.Xsd))
        | "System.Float" -> Some(XmlQualifiedName("float", XmlNamespace.Xsd))
        | "System.Int32" -> Some(XmlQualifiedName("int", XmlNamespace.Xsd))
        | "System.Numerics.BigInteger" -> Some(XmlQualifiedName("integer", XmlNamespace.Xsd))
        | "System.Int64" -> Some(XmlQualifiedName("long", XmlNamespace.Xsd))
        | _ -> None

type internal ContentType =
    | FileStorage of FileInfo
    | Data of byte[]

type public ContentEncoding =
    | Binary = 0
    | Base64 = 1

[<AllowNullLiteral>]
type public BinaryContent internal (contentID: string, content: ContentType) =
    member val ContentEncoding = ContentEncoding.Binary with get, set
    member __.ContentID
        with get() =
            match contentID with
            | null | "" -> XRoadHelper.generateNonce()
            | _ -> contentID
    member __.OpenStream() : Stream =
        match content with
        | FileStorage(file) -> upcast file.OpenRead()
        | Data(data) -> upcast new MemoryStream(data)
    member __.GetBytes() =
        match content with
        | FileStorage(file) -> File.ReadAllBytes(file.FullName)
        | Data(data) -> data
    static member Create(file) = BinaryContent("", FileStorage(file))
    static member Create(contentID, file) = BinaryContent(contentID, FileStorage(file))
    static member Create(data) = BinaryContent("", Data(data))
    static member Create(contentID, data) = BinaryContent(contentID, Data(data))

type SoapHeaderValue(name: XmlQualifiedName, value: obj, required: bool) =
    member val Name = name with get
    member val Value = value with get
    member val IsRequired = required with get

type XRoadMessage() =
    member val Header: SoapHeaderValue array = [||] with get, set
    member val Body: (XmlQualifiedName * obj) array = [||] with get, set
    member val Attachments = Dictionary<string, BinaryContent>() with get, set
    member this.GetPart(name) =
        this.Body
        |> Array.tryFind (fst >> ((=) name))
        |> Option.map (snd)
        |> Option.fold (fun _ x -> x) null

type XRoadRequestOptions(uri: string, isEncoded: bool, isMultipart: bool, protocol: XRoadProtocol, types: Type[]) =
    member val IsEncoded = isEncoded with get
    member val IsMultipart = isMultipart with get
    member val Protocol = protocol with get
    member val Uri = uri with get
    member val Types = types with get
    member val Accessor: XmlQualifiedName = null with get, set

type XRoadResponseOptions(isEncoded: bool, isMultipart: bool, protocol: XRoadProtocol, types: IDictionary<XmlQualifiedName, Type>) =
    member val IsEncoded = isEncoded with get
    member val IsMultipart = isMultipart with get
    member val Protocol = protocol with get
    member val Types = types with get
    member val Accessor: XmlQualifiedName = null with get, set
    member val ExpectUnexpected = false with get, set

[<AllowNullLiteral>]
type SerializerContext() =
    let attachments = Dictionary<string, BinaryContent>()
    member val IsMultipart = false with get, set
    member val Attachments = attachments with get
    member this.AddAttachments(attachments: IDictionary<_,_>) =
        match attachments with
        | null -> ()
        | _ -> attachments |> Seq.iter (fun kvp -> this.Attachments.Add(kvp.Key, kvp.Value))
    member __.GetAttachment(href: string) =
        if href.StartsWith("cid:") then
            let contentID = href.Substring(4)
            match attachments.TryGetValue(contentID) with
            | true, value -> value
            | _ -> failwithf "Multipart message doesn't contain content part with ID `%s`." contentID
        else failwithf "Invalid multipart content reference: `%s`." href

[<AbstractClass; Sealed>]
type BinaryContentHelper private () =
    static member DeserializeBinaryContent(reader: XmlReader, context: SerializerContext) =
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            match reader.GetAttribute("href") with
            | null ->
                if reader.IsEmptyElement then BinaryContent.Create([| |])
                else
                    reader.Read() |> ignore
                    let bufferSize = 4096
                    let buffer = Array.zeroCreate<byte>(bufferSize)
                    use stream = new MemoryStream()
                    let rec readContents() =
                        let readCount = reader.ReadContentAsBase64(buffer, 0, bufferSize)
                        if readCount > 0 then stream.Write(buffer, 0, readCount)
                        if readCount = bufferSize then readContents()
                    readContents()
                    stream.Flush()
                    stream.Position <- 0L
                    BinaryContent.Create(stream.ToArray())
            | contentID -> context.GetAttachment(contentID)

    static member DeserializeXopBinaryContent(reader: XmlReader, context: SerializerContext) =
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            if reader.IsEmptyElement then BinaryContent.Create([| |])
            else
                let depth = reader.Depth + 1
                let rec moveToXopInclude () =
                    if reader.Read() then
                        if reader.NodeType = XmlNodeType.EndElement && reader.Depth < depth then false
                        elif reader.NodeType <> XmlNodeType.Element || reader.Depth <> depth || reader.LocalName <> "Include" || reader.NamespaceURI <> XmlNamespace.Xop then moveToXopInclude()
                        else true
                    else false
                if moveToXopInclude () then
                    match reader.GetAttribute("href") with
                    | null -> failwithf "Missing reference to multipart content in xop:Include element."
                    | contentID -> context.GetAttachment(contentID)
                else BinaryContent.Create([| |])

    static member SerializeBinaryContent(writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            let content = unbox<BinaryContent> value
            if context.IsMultipart then
                context.Attachments.Add(content.ContentID, content)
                writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            else
                let bytes = (unbox<BinaryContent> value).GetBytes()
                writer.WriteBase64(bytes, 0, bytes.Length)

    static member SerializeXopBinaryContent(writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            writer.WriteStartElement("xop", "Include", XmlNamespace.Xop)
            let content = unbox<BinaryContent> value
            context.Attachments.Add(content.ContentID, content)
            writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            writer.WriteEndElement()
