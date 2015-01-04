module XRoadTypeProvider.TypeBuilder

open System.Xml

[<Literal>]
let XrdNamespace = "http://x-road.ee/xsd/x-road.xsd"

[<Literal>]
let XteeNamespace = "http://x-tee.riik.ee/xsd/xtee.xsd"

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
    | XrdNamespace -> mapXrdType qn.Name
    | _ -> failwithf "Unmapped type name %O" qn

let resolveElementType (qn: XmlQualifiedName) tns =
    match qn.Namespace with
    | XrdNamespace -> mapXrdElementType qn.Name
    | XteeNamespace -> mapXteeElementType qn.Name
    | ns when ns = tns -> typeof<obj>
    | _ -> failwithf "Unmapped element name %O" qn
