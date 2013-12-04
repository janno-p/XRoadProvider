module XteeTypeProvider.Xtee

open System.Web.Services.Description
open System.Xml

[<Literal>]
let XteeNamespace = "http://x-tee.riik.ee/xsd/xtee.xsd"

let GetOperationVersion (operation : OperationBinding) =
    [ for ext in operation.Extensions -> ext ]
    |> Seq.choose (fun extension ->
        match extension with
        | :? XmlElement as element ->
            match (element.NamespaceURI, element.LocalName) with
            | (XteeNamespace, "version") -> Some element.InnerText
            | _ -> None
        | _ -> None)
    |> Seq.exactlyOne
