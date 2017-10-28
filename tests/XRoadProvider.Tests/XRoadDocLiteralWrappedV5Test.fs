namespace XRoadProvider.Test

(*
open FsUnit
open NUnit.Framework
open XRoad
open XRoad.Wsdl

[<TestFixture>]
module XRoadDocLiteralWrappedV5Test =
    [<Test>]
    let ``Read aktorstest service`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        services.Length |> should equal 1
        let service = services |> List.head
        service.Name |> should equal "aktorstestService"
        service.Ports.Length |> should equal 1
        let port = service.Ports |> List.head
        port.Uri |> should equal "http://localhost:8080/axis2/services/aktorstestService"
        port.Documentation |> should equal (Some "Test andmekogu xtee ver5 doc/literal stiili jaoks")
        port.MessageProtocol |> should equal (Version31Ee "aktorstest")

    [<Test>]
    let ``Parse multipart input operation`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "fileUpload")
        operation.Documentation |> should equal (Some "Faili üleslaadimine")
        operation.Version |> should equal (Some "v1")
        match operation.InputParameters with DocLiteralWrapped(_) -> () | _ -> failwith "Invalid operation style"
        operation.InputParameters.Parameters.Length |> should equal 1
        operation.InputParameters.RequiredHeaders.Length |> should equal 5
        operation.InputParameters.IsMultipart |> should be True
        operation.InputParameters.IsEncoded |> should be False
        operation.OutputParameters.Parameters.Length |> should equal 1
        operation.OutputParameters.RequiredHeaders.Length |> should equal 5
        operation.OutputParameters.IsEncoded |> should be False
        operation.OutputParameters.IsMultipart |> should be False
        let context = TypeBuilderContext.FromSchema(schema, "et")
        context.MessageProtocol |> should equal (Version31Ee "aktorstest")

    [<Test>]
    let ``Parse multipart output operation`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "fileDownload")
        operation.Documentation |> should equal (Some "Faili allalaadimine")
        operation.Version |> should equal (Some "v1")
        operation.InputParameters.Parameters.Length |> should equal 1
        operation.InputParameters.RequiredHeaders.Length |> should equal 5
        operation.InputParameters.IsMultipart |> should be False
        operation.OutputParameters.Parameters.Length |> should equal 1
        operation.OutputParameters.RequiredHeaders.Length |> should equal 5
        operation.OutputParameters.IsMultipart |> should be True

    [<Test>]
    let ``Parse operation without version number`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "listMethods")
        operation.Documentation |> should equal None
        operation.Version |> should equal None
        operation.InputParameters.Parameters.Length |> should equal 1
        operation.InputParameters.RequiredHeaders.Length |> should equal 0
        operation.InputParameters.IsMultipart |> should be False
        operation.OutputParameters.Parameters.Length |> should equal 1
        operation.OutputParameters.RequiredHeaders.Length |> should equal 0
        operation.OutputParameters.IsMultipart |> should be False

    [<Test>]
    let ``Parse non-multipart operation`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "isikOtsing")
        operation.Name |> should equal "isikOtsing"
        operation.Documentation |> should equal (Some "Isiku andmete otsimine isikukoodi järgi")
        operation.Version |> should equal (Some "v1")
        operation.InputParameters.Parameters.Length |> should equal 1
        operation.InputParameters.RequiredHeaders.Length |> should equal 5
        operation.InputParameters.IsMultipart |> should be False
        operation.OutputParameters.Parameters.Length |> should equal 1
        operation.OutputParameters.RequiredHeaders.Length |> should equal 5
        operation.OutputParameters.IsMultipart |> should be False

    [<Test>]
    let ``Parse Aktorstest xml schema definition`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let typeSchemas = schema.TypeSchemas
        typeSchemas.Count |> should equal 2
        typeSchemas.ContainsKey "http://aktorstest.x-road.ee/producer" |> should be True
        typeSchemas.ContainsKey "http://x-road.ee/xsd/x-road.xsd" |> should be True
        let mainSchema = typeSchemas.["http://aktorstest.x-road.ee/producer"]
        mainSchema.TargetNamespace.NamespaceName |> should equal "http://aktorstest.x-road.ee/producer"
        mainSchema.QualifiedAttributes |> should be False
        mainSchema.QualifiedElements |> should be False
        mainSchema.Elements.Count |> should equal 18
        mainSchema.Types.Count |> should equal 4
*)

do ()
