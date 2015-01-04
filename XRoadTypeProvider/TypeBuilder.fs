module XRoadTypeProvider.TypeBuilder

open System.Xml

[<Literal>]
let XrdNamespace = "http://x-road.ee/xsd/x-road.xsd"

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

let resolveType (qn: XmlQualifiedName) =
    match qn.Namespace with
    | XrdNamespace -> mapXrdType qn.Name
    | _ -> failwithf "Unmapped type name %O" qn

let resolveElementType (qn: XmlQualifiedName) tns =
    match qn.Namespace with
    | XrdNamespace -> mapXrdElementType qn.Name
    | ns when ns = tns -> typeof<obj>
    | _ -> failwithf "Unmapped element name %O" qn
