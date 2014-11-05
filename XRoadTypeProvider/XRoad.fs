module XRoadTypeProvider.XRoad

open System.Web.Services.Description
open System.Xml

[<Literal>]
let XRoadOldNamespace = "http://x-tee.riik.ee/xsd/xtee.xsd"

[<Literal>]
let XRoadNewNamespace = "http://x-road.ee/xsd/x-road.xsd"

let GetOperationVersion (operation : OperationBinding) =
    [ for ext in operation.Extensions -> ext ]
    |> Seq.choose (fun extension ->
        match extension with
        | :? XmlElement as element ->
            match (element.NamespaceURI, element.LocalName) with
            | (XRoadOldNamespace, "version")
            | (XRoadNewNamespace, "version") -> Some element.InnerText
            | _ -> None
        | _ -> None)
    |> Seq.exactlyOne
