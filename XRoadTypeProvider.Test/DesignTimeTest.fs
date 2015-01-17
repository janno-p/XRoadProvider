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
        (*
        let schema = XRoadTypeProvider.Wsdl.readDescription(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let portType = [for pt in schema.PortTypes -> pt] |> List.tryFind (fun pt -> pt.Name = "TestPortType")
        let binding = [for b in schema.Bindings -> b] |> List.tryFind (fun b -> b.Name = "TestSoapBinding")
        let operation = [for op in binding.Value.Operations -> op] |> List.tryFind (fun op -> op.Name = "fileUpload")
        let result = parseOperationDetails schema portType.Value operation.Value
        test <@ result.Name = "fileUpload" @>
        test <@ result.Documentation.Count = 2 @>
        test <@ result.Documentation.ContainsKey("en") && result.Documentation.["en"] = "File Upload" @>
        test <@ result.Documentation.ContainsKey("et") && result.Documentation.["et"] = "Faili üleslaadimine" @>
        test <@ result.Version = Some "v1" @>
        test <@ result.Request.Body.Length = 1 @>
        test <@ result.Request.Header.Length = 5 @>
        test <@ result.Request.MultipartContent.Length = 1 @>
        test <@ result.Response.Body.Length = 1 @>
        test <@ result.Response.Header.Length = 5 @>
        test <@ result.Response.MultipartContent.Length = 0 @>
        *)
        ()

    [<Test>]
    let ``Parse multipart output operation`` () =
        (*
        let schema = XRoadTypeProvider.Wsdl.readDescription(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let portType = [for pt in schema.PortTypes -> pt] |> List.tryFind (fun pt -> pt.Name = "TestPortType")
        let binding = [for b in schema.Bindings -> b] |> List.tryFind (fun b -> b.Name = "TestSoapBinding")
        let operation = [for op in binding.Value.Operations -> op] |> List.tryFind (fun op -> op.Name = "fileDownload")
        let result = parseOperationDetails schema portType.Value operation.Value
        test <@ result.Name = "fileDownload" @>
        test <@ result.Documentation.Count = 2 @>
        test <@ result.Documentation.ContainsKey("en") && result.Documentation.["en"] = "File download" @>
        test <@ result.Documentation.ContainsKey("et") && result.Documentation.["et"] = "Faili allalaadimine" @>
        test <@ result.Version = Some "v1" @>
        test <@ result.Request.Body.Length = 1 @>
        test <@ result.Request.Header.Length = 5 @>
        test <@ result.Request.MultipartContent.Length = 0 @>
        test <@ result.Response.Body.Length = 1 @>
        test <@ result.Response.Header.Length = 5 @>
        test <@ result.Response.MultipartContent.Length = 1 @>
        *)
        ()

    [<Test>]
    let ``Parse operation without version number`` () =
        (*
        let schema = XRoadTypeProvider.Wsdl.readDescription(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let portType = [for pt in schema.PortTypes -> pt] |> List.tryFind (fun pt -> pt.Name = "TestPortType")
        let binding = [for b in schema.Bindings -> b] |> List.tryFind (fun b -> b.Name = "TestSoapBinding")
        let operation = [for op in binding.Value.Operations -> op] |> List.tryFind (fun op -> op.Name = "listMethods")
        let result = parseOperationDetails schema portType.Value operation.Value
        test <@ result.Name = "listMethods" @>
        test <@ result.Documentation.Count = 1 @>
        test <@ result.Documentation.ContainsKey("en") && result.Documentation.["en"] = "listMethods" @>
        test <@ result.Version.IsNone @>
        test <@ result.Request.Body.Length = 1 @>
        test <@ result.Request.Header.Length = 0 @>
        test <@ result.Request.MultipartContent.Length = 0 @>
        test <@ result.Response.Body.Length = 1 @>
        test <@ result.Response.Header.Length = 0 @>
        test <@ result.Response.MultipartContent.Length = 0 @>
        *)
        ()

    [<Test>]
    let ``Parse non-multipart operation`` () =
        (*
        let schema = XRoadTypeProvider.Wsdl.readDescription(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml")
        let portType = [for pt in schema.PortTypes -> pt] |> List.tryFind (fun pt -> pt.Name = "TestPortType")
        let binding = [for b in schema.Bindings -> b] |> List.tryFind (fun b -> b.Name = "TestSoapBinding")
        let operation = [for op in binding.Value.Operations -> op] |> List.tryFind (fun op -> op.Name = "isikOtsing")
        let result = parseOperationDetails schema portType.Value operation.Value
        test <@ result.Name = "isikOtsing" @>
        test <@ result.Documentation.Count = 2 @>
        test <@ result.Documentation.ContainsKey("en") && result.Documentation.["en"] = "Search person by Id-code" @>
        test <@ result.Documentation.ContainsKey("et") && result.Documentation.["et"] = "Isiku andmete otsimine isikukoodi järgi" @>
        test <@ result.Version = Some "v1" @>
        test <@ result.Request.Body.Length = 1 @>
        test <@ result.Request.Header.Length = 5 @>
        test <@ result.Request.MultipartContent.Length = 0 @>
        test <@ result.Response.Body.Length = 1 @>
        test <@ result.Response.Header.Length = 5 @>
        test <@ result.Response.MultipartContent.Length = 0 @>
        *)
        ()
