namespace XRoad

open System
open System.Collections.Generic
open System.IO
open System.Xml

[<RequireQualifiedAccessAttribute>]
module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xrd = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"

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

type SoapHeaderValue(name: XmlQualifiedName, value: obj, required: bool) =
    member val Name = name with get
    member val Value = value with get
    member val IsRequired = required with get

type XRoadMessage() =
    member val Header: SoapHeaderValue array = [||] with get, set
    member val Body: (XmlQualifiedName * obj) array = [||] with get, set
    member val Attachments = Dictionary<string, Stream>() with get, set
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
