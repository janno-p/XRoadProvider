module XRoadTypeProvider.Wsdl

open System
open System.IO
open System.Web.Services.Description
open System.Xml

module XmlNamespace =
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnvelope = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"


let private mapXrdType = function
    | "faultCode"
    | "faultString" -> typeof<string>
    | n             -> failwithf "Unmapped XRD type %s" n

let private mapXrdElementType = function
    | "async" -> typeof<bool>
    | "address"
    | "authenticator"
    | "consumer"
    | "encode"
    | "id"
    | "issue"
    | "nocontent"
    | "notes"
    | "position"
    | "producer"
    | "ref"
    | "requirecontent"
    | "service"
    | "technotes"
    | "title"
    | "unit"
    | "userId"
    | "userName"
    | "version"
    | "wildcard" -> typeof<string>
    | "listMethods"
    | "listMethodsResponse"
    | "testSystem"
    | "testSystemResponse"
    | "loadClassification"
    | "loadClassificationResponse"
    | "userAllowedMethods"
    | "userAllowedMethodsResponse" -> typeof<obj>
    // HACK: these are really complexTypes
    | "unitRepresent"
    | "unitRepresentResponse"
    | "unitValid"
    | "unitValidResponse" -> typeof<obj>
    | n -> failwithf "Unmapped XRD element type %s" n

let mapXteeElementType = function
    | "asynkroonne" -> typeof<bool>
    | "allasutus"
    | "amet"
    | "ametnik"
    | "ametniknimi"
    | "andmekogu"
    | "asutus"
    | "autentija"
    | "id"
    | "isikukood"
    | "nimi"
    | "nocontent"
    | "notes"
    | "ref"
    | "requirecontent"
    | "title"
    | "technotes"
    | "toimik"
    | "version"
    | "wildcard" -> typeof<string>
    | "address"
    | "complex" -> typeof<obj>
    | x -> failwithf "Unmapped XRD element type %s" x

let resolveType (qn: XmlQualifiedName) =
    match qn.Namespace with
    | XmlNamespace.XRoad -> mapXrdType qn.Name
    | _ -> failwithf "Unmapped type name %O" qn

let resolveElementType (qn: XmlQualifiedName) tns =
    match qn.Namespace with
    | XmlNamespace.XRoad -> mapXrdElementType qn.Name
    | XmlNamespace.Xtee -> mapXteeElementType qn.Name
    | ns when ns = tns -> typeof<obj>
    | _ -> failwithf "Unmapped element name %O" qn

let Resolve uri =
    match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
    | true -> uri
    | _ ->
        let fullPath = (new FileInfo(uri)).FullName
        match File.Exists(fullPath) with
        | true -> fullPath
        | _ -> failwith (sprintf "Cannot resolve url location `%s`" uri)

type ServicePort = {
    Address: string
    Producer: string
    Documentation: string
}

let parseServicePort (port: Port) lang =
    let rec parseExtensions (exts: obj list) sp =
        match exts with
        | [] -> sp
        | ext::exts ->
            match ext with
            | :? SoapAddressBinding as addr -> parseExtensions exts { sp with Address = addr.Location }
            | :? XmlElement as el ->
                match el.LocalName, el.NamespaceURI with
                | "address", XmlNamespace.Xtee
                | "address", XmlNamespace.XRoad ->
                    match [for a in el.Attributes -> a] |> Seq.tryFind (fun a -> a.LocalName = "producer") with
                    | Some a -> parseExtensions exts { sp with Producer = a.Value }
                    | _ -> parseExtensions exts sp
                | "title", XmlNamespace.Xtee
                | "title", XmlNamespace.XRoad ->
                    match [for a in el.Attributes -> a] |> Seq.tryFind (fun a -> a.LocalName = "lang" && a.NamespaceURI = "http://www.w3.org/XML/1998/namespace") with
                    | Some a when a.Value = lang -> parseExtensions exts { sp with Documentation = el.InnerText }
                    | _ -> match sp.Documentation with
                           | "" -> parseExtensions exts { sp with Documentation = el.InnerText }
                           | _ -> parseExtensions exts sp
                | _ -> parseExtensions exts sp
            | _ -> parseExtensions exts sp
    let defaultServicePort = { Address = ""; Producer = ""; Documentation = "" }
    parseExtensions [for e in port.Extensions -> e] defaultServicePort

let ReadDescription (uri : string) =
    use reader = XmlReader.Create(uri)
    ServiceDescription.Read(reader, true)
