namespace XRoadProvider.Test

open NUnit.Framework
open Swensen.Unquote
open System.IO
open XRoadProvider.Wsdl

[<TestFixture>]
module XRoadDocLiteralWrappedV5Test =
    [<Test>]
    let ``Read aktorstest service`` () =
        let schema = readSchema(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let services = schema.Services
        test <@ services.Length = 1 @>
        let service = services |> List.head
        test <@ service.Name = "aktorstestService" @>
        test <@ service.Ports.Length = 1 @>
        let port = service.Ports |> List.head
        test <@ port.Address = "http://localhost:8080/axis2/services/aktorstestService" @>
        test <@ port.Documentation.Count = 2 @>
        test <@ port.Documentation.ContainsKey("en") @>
        test <@ port.Documentation.["en"] = "Test database for xroad ver.5 doc/literal style" @>
        test <@ port.Documentation.ContainsKey("et") @>
        test <@ port.Documentation.["et"] = "Test andmekogu xtee ver5 doc/literal stiili jaoks" @>
        test <@ port.Producer = "aktorstest" @>

    [<Test>]
    let ``Parse multipart input operation`` () =
        let schema = readSchema(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name.LocalName = "fileUpload")
        test <@ operation.Documentation.Count = 2 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "File Upload" @>
        test <@ operation.Documentation.ContainsKey("et") && operation.Documentation.["et"] = "Faili üleslaadimine" @>
        test <@ operation.Style = DocLiteral @>
        test <@ operation.Version = Some "v1" @>
        test <@ operation.Request.Body.Length = 1 @>
        test <@ operation.Request.Header.Length = 5 @>
        test <@ operation.Request.MultipartContent.Length = 1 @>
        test <@ operation.Response.Body.Length = 1 @>
        test <@ operation.Response.Header.Length = 5 @>
        test <@ operation.Response.MultipartContent.Length = 0 @>
        ()

    [<Test>]
    let ``Parse multipart output operation`` () =
        let schema = readSchema(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name.LocalName = "fileDownload")
        test <@ operation.Documentation.Count = 2 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "File download" @>
        test <@ operation.Documentation.ContainsKey("et") && operation.Documentation.["et"] = "Faili allalaadimine" @>
        test <@ operation.Style = DocLiteral @>
        test <@ operation.Version = Some "v1" @>
        test <@ operation.Request.Body.Length = 1 @>
        test <@ operation.Request.Header.Length = 5 @>
        test <@ operation.Request.MultipartContent.Length = 0 @>
        test <@ operation.Response.Body.Length = 1 @>
        test <@ operation.Response.Header.Length = 5 @>
        test <@ operation.Response.MultipartContent.Length = 1 @>
        ()

    [<Test>]
    let ``Parse operation without version number`` () =
        let schema = readSchema(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name.LocalName = "listMethods")
        test <@ operation.Documentation.Count = 1 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "listMethods" @>
        test <@ operation.Style = DocLiteral @>
        test <@ operation.Version.IsNone @>
        test <@ operation.Request.Body.Length = 1 @>
        test <@ operation.Request.Header.Length = 0 @>
        test <@ operation.Request.MultipartContent.Length = 0 @>
        test <@ operation.Response.Body.Length = 1 @>
        test <@ operation.Response.Header.Length = 0 @>
        test <@ operation.Response.MultipartContent.Length = 0 @>
        ()

    [<Test>]
    let ``Parse non-multipart operation`` () =
        let schema = readSchema(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name.LocalName = "isikOtsing")
        test <@ operation.Name.LocalName = "isikOtsing" @>
        test <@ operation.Documentation.Count = 2 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "Search person by Id-code" @>
        test <@ operation.Documentation.ContainsKey("et") && operation.Documentation.["et"] = "Isiku andmete otsimine isikukoodi järgi" @>
        test <@ operation.Style = DocLiteral @>
        test <@ operation.Version = Some "v1" @>
        test <@ operation.Request.Body.Length = 1 @>
        test <@ operation.Request.Header.Length = 5 @>
        test <@ operation.Request.MultipartContent.Length = 0 @>
        test <@ operation.Response.Body.Length = 1 @>
        test <@ operation.Response.Header.Length = 5 @>
        test <@ operation.Response.MultipartContent.Length = 0 @>
        ()

    [<Test>]
    let ``Parse Aktorstest xml schema definition`` () =
        let schema = readSchema(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let typeSchemas = schema.TypeSchemas
        test <@ typeSchemas.Length = 1 @>
        let mainSchema = typeSchemas.[0]
        test <@ mainSchema.TargetNamespace.NamespaceName = "http://aktorstest.x-road.ee/producer" @>
        test <@ mainSchema.QualifiedAttributes = false @>
        test <@ mainSchema.QualifiedElements = false @>
        test <@ mainSchema.Imports.Length = 2 @>
        let ns1, uri1 = mainSchema.Imports.[1]
        test <@ ns1.NamespaceName = "http://x-road.ee/xsd/x-road.xsd" @>
        test <@ uri1.IsSome && uri1.Value.ToString() = "http://x-road.ee/xsd/x-road.xsd" @>
        let ns2, uri2 = mainSchema.Imports.[0]
        test <@ ns2.NamespaceName = "http://www.w3.org/2005/05/xmlmime" @>
        test <@ uri2.IsSome && uri2.Value.ToString() = "http://www.w3.org/2005/05/xmlmime" @>
        test <@ mainSchema.Includes.Length = 0 @>
        test <@ mainSchema.Elements.Count = 14 @>
        test <@ mainSchema.Types.Count = 4 @>
