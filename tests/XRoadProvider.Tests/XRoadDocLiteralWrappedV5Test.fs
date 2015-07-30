namespace XRoadProvider.Test

open NUnit.Framework
open Swensen.Unquote
open System.IO
open XRoad
open XRoad.ServiceDescription

[<TestFixture>]
module XRoadDocLiteralWrappedV5Test =
    [<Test>]
    let ``Read aktorstest service`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        test <@ services.Length = 1 @>
        let service = services |> List.head
        test <@ service.Name = "aktorstestService" @>
        test <@ service.Ports.Length = 1 @>
        let port = service.Ports |> List.head
        test <@ port.Uri = "http://localhost:8080/axis2/services/aktorstestService" @>
        test <@ port.Documentation.IsSome @>
        test <@ port.Documentation.Value = "Test andmekogu xtee ver5 doc/literal stiili jaoks" @>
        test <@ port.Producer = "aktorstest" @>

    [<Test>]
    let ``Parse multipart input operation`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "fileUpload")
        test <@ operation.Documentation.IsSome @>
        test <@ operation.Documentation.Value = "Faili üleslaadimine" @>
        test <@ operation.Version = Some "v1" @>
        test <@ operation.InputParameters.Parameters.Length = 1 @>
        test <@ operation.InputParameters.RequiredHeaders.Length = 5 @>
        test <@ operation.InputParameters.IsMultipart = true @>
        test <@ operation.InputParameters.IsEncoded = false @>
        test <@ operation.InputParameters.Accessor.IsNone @>
        test <@ operation.OutputParameters.Parameters.Length = 1 @>
        test <@ operation.OutputParameters.RequiredHeaders.Length = 5 @>
        test <@ operation.OutputParameters.IsEncoded = false @>
        test <@ operation.OutputParameters.IsMultipart = false @>
        test <@ operation.OutputParameters.Accessor.IsNone @>
        let context = TypeBuilderContext.FromSchema(schema, "et")
        let expectedProtocol = XRoad.Common.CodeSpec.XRoadProtocol.Version_31
        test <@ context.Protocol = expectedProtocol @>

    [<Test>]
    let ``Parse multipart output operation`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "fileDownload")
        test <@ operation.Documentation.IsSome @>
        test <@ operation.Documentation.Value = "Faili allalaadimine" @>
        test <@ operation.Version = Some "v1" @>
        test <@ operation.InputParameters.Parameters.Length = 1 @>
        test <@ operation.InputParameters.RequiredHeaders.Length = 5 @>
        test <@ operation.InputParameters.IsMultipart = false @>
        test <@ operation.OutputParameters.Parameters.Length = 1 @>
        test <@ operation.OutputParameters.RequiredHeaders.Length = 5 @>
        test <@ operation.OutputParameters.IsMultipart = true @>
        ()

    [<Test>]
    let ``Parse operation without version number`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "listMethods")
        test <@ operation.Documentation.IsNone @>
        test <@ operation.Version.IsNone @>
        test <@ operation.InputParameters.Parameters.Length = 1 @>
        test <@ operation.InputParameters.RequiredHeaders.Length = 0 @>
        test <@ operation.InputParameters.IsMultipart = false @>
        test <@ operation.OutputParameters.Parameters.Length = 1 @>
        test <@ operation.OutputParameters.RequiredHeaders.Length = 0 @>
        test <@ operation.OutputParameters.IsMultipart = false @>
        ()

    [<Test>]
    let ``Parse non-multipart operation`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let services = schema.Services
        let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "isikOtsing")
        test <@ operation.Name = "isikOtsing" @>
        test <@ operation.Documentation.IsSome @>
        test <@ operation.Documentation.Value = "Isiku andmete otsimine isikukoodi järgi" @>
        test <@ operation.Version = Some "v1" @>
        test <@ operation.InputParameters.Parameters.Length = 1 @>
        test <@ operation.InputParameters.RequiredHeaders.Length = 5 @>
        test <@ operation.InputParameters.IsMultipart = false @>
        test <@ operation.OutputParameters.Parameters.Length = 1 @>
        test <@ operation.OutputParameters.RequiredHeaders.Length = 5 @>
        test <@ operation.OutputParameters.IsMultipart = false @>
        ()

    [<Test>]
    let ``Parse Aktorstest xml schema definition`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/AktorstestService.wsdl.xml", "et")
        let typeSchemas = schema.TypeSchemas
        test <@ typeSchemas.Count = 2 @>
        test <@ typeSchemas.ContainsKey "http://aktorstest.x-road.ee/producer" @>
        test <@ typeSchemas.ContainsKey "http://x-road.ee/xsd/x-road.xsd" @>
        let mainSchema = typeSchemas.["http://aktorstest.x-road.ee/producer"]
        test <@ mainSchema.TargetNamespace.NamespaceName = "http://aktorstest.x-road.ee/producer" @>
        test <@ mainSchema.QualifiedAttributes = false @>
        test <@ mainSchema.QualifiedElements = false @>
        test <@ mainSchema.Elements.Count = 18 @>
        test <@ mainSchema.Types.Count = 4 @>
