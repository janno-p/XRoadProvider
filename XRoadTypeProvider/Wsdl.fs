module XRoadTypeProvider.Wsdl

open System
open System.IO
open System.Web.Services.Description
open System.Xml

let Resolve uri =
    match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
    | true -> uri
    | _ ->
        let fullPath = (new FileInfo(uri)).FullName
        match File.Exists(fullPath) with
        | true -> fullPath
        | _ -> failwith (sprintf "Cannot resolve url location `%s`" uri)

let ReadDescription (uri : string) =
    use reader = XmlReader.Create(uri)
    ServiceDescription.Read(reader, true)
