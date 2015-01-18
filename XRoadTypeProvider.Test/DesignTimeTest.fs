namespace XRoadTypeProvider.Test

open NUnit.Framework
open Swensen.Unquote
open System.IO
open XRoadTypeProvider.DesignTime
open XRoadTypeProvider.Wsdl

[<TestFixture>]
module DesignTime =
    [<Test>]
    let ``Read aktorstest service`` () =
        let services = readServices(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
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
        let services = readServices(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name = "fileUpload")
        test <@ operation.Documentation.Count = 2 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "File Upload" @>
        test <@ operation.Documentation.ContainsKey("et") && operation.Documentation.["et"] = "Faili üleslaadimine" @>
        test <@ operation.Style = XRoadTypeProvider.XRoad.XRoadBindingStyle.DocumentLiteral @>
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
        let services = readServices(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name = "fileDownload")
        test <@ operation.Documentation.Count = 2 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "File download" @>
        test <@ operation.Documentation.ContainsKey("et") && operation.Documentation.["et"] = "Faili allalaadimine" @>
        test <@ operation.Style = XRoadTypeProvider.XRoad.XRoadBindingStyle.DocumentLiteral @>
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
        let services = readServices(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name = "listMethods")
        test <@ operation.Documentation.Count = 1 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "listMethods" @>
        test <@ operation.Style = XRoadTypeProvider.XRoad.XRoadBindingStyle.DocumentLiteral @>
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
        let services = readServices(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let operation = services.Head.Ports.Head.Operations |> List.find (fun op -> op.Name = "isikOtsing")
        test <@ operation.Name = "isikOtsing" @>
        test <@ operation.Documentation.Count = 2 @>
        test <@ operation.Documentation.ContainsKey("en") && operation.Documentation.["en"] = "Search person by Id-code" @>
        test <@ operation.Documentation.ContainsKey("et") && operation.Documentation.["et"] = "Isiku andmete otsimine isikukoodi järgi" @>
        test <@ operation.Style = XRoadTypeProvider.XRoad.XRoadBindingStyle.DocumentLiteral @>
        test <@ operation.Version = Some "v1" @>
        test <@ operation.Request.Body.Length = 1 @>
        test <@ operation.Request.Header.Length = 5 @>
        test <@ operation.Request.MultipartContent.Length = 0 @>
        test <@ operation.Response.Body.Length = 1 @>
        test <@ operation.Response.Header.Length = 5 @>
        test <@ operation.Response.MultipartContent.Length = 0 @>
        ()
