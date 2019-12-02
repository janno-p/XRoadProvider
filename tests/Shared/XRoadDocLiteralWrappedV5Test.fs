module XRoadProvider.Tests.XRoadDocLiteralWrappedV5Test

open Expecto
open System
open XRoad
open XRoad.Wsdl

let [<Tests>] tests =
    testList "doc/literal wrapped v5 tests" [
        test "read aktorstest service" {
            let schema = ProducerDescription.Load(Uri(__SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"), "et", [])
            let services = schema.Services
            Expect.equal services.Length 1 "should find exactly 1 service definition"
            let service = services |> List.head
            Expect.equal service.Name "aktorstestService" "aktorstest service not found"
            Expect.equal service.Ports.Length 1 "should have exactly 1 port definition"
            let port = service.Ports |> List.head
            Expect.equal port.Uri "http://localhost:8080/axis2/services/aktorstestService" "invalid port address"
            Expect.equal port.Documentation (Some "Test andmekogu xtee ver5 doc/literal stiili jaoks") "documentation not parsed"
            Expect.equal port.MessageProtocol (Version31Ee "aktorstest") "invalid protocol version"
        }
    
        test "parse multipart input operation" {
            let schema = ProducerDescription.Load(Uri(__SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"), "et", [])
            let services = schema.Services
            let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "fileUpload")
            Expect.equal operation.Documentation (Some "Faili üleslaadimine") "invalid documentation"
            Expect.equal operation.Version (Some "v1") "invalid operation version"
            match operation.InputParameters with DocLiteralWrapped(_) -> () | _ -> failtest "invalid operation input style"
            Expect.equal operation.InputParameters.Parameters.Length 1 "should have 1 parameter"
            Expect.equal operation.InputParameters.RequiredHeaders.Length 5 "should have 5 required headers"
            Expect.isTrue operation.InputParameters.IsMultipart "should have multipart binding"
            Expect.isFalse operation.InputParameters.IsEncoded "should be literal style"
            Expect.equal operation.OutputParameters.Parameters.Length 1 "should have 1 result"
            Expect.equal operation.OutputParameters.RequiredHeaders.Length 5 "should have 5 required headers in response"
            Expect.isFalse operation.OutputParameters.IsEncoded "should be literal style response"
            Expect.isFalse operation.OutputParameters.IsMultipart "should not have multipart binding in response"
            let context = TypeBuilderContext.FromSchema(schema)
            Expect.equal context.MessageProtocol (Version31Ee "aktorstest") "wrong message protocol version"
        }
    
        test "parse multipart output operation" {
            let schema = ProducerDescription.Load(Uri(__SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"), "et", [])
            let services = schema.Services
            let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "fileDownload")
            Expect.equal operation.Documentation (Some "Faili allalaadimine") "invalid documentation"
            Expect.equal operation.Version (Some "v1") "invalid operation version"
            Expect.equal operation.InputParameters.Parameters.Length 1 "wrong number of input parameters"
            Expect.equal operation.InputParameters.RequiredHeaders.Length 5 "wrong number of required headers in input"
            Expect.isFalse operation.InputParameters.IsMultipart "should not be multipart message"
            Expect.equal operation.OutputParameters.Parameters.Length 1 "wrong number of output parameters"
            Expect.equal operation.OutputParameters.RequiredHeaders.Length 5 "wrong number of required headers in output"
            Expect.isTrue operation.OutputParameters.IsMultipart "should have multipart response binding"
        }
    
        test "parse operation without version number" {
            let schema = ProducerDescription.Load(Uri(__SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"), "et", [])
            let services = schema.Services
            let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "listMethods")
            Expect.isNone operation.Documentation "should not have documentation"
            Expect.isNone operation.Version "should not have version"
            Expect.equal operation.InputParameters.Parameters.Length 1 "wrong number of input parameters"
            Expect.equal operation.InputParameters.RequiredHeaders.Length 0 "should not have required headers for input"
            Expect.isFalse operation.InputParameters.IsMultipart "should not have multipart input binding"
            Expect.equal operation.OutputParameters.Parameters.Length 1 "wrong number of output parameters"
            Expect.equal operation.OutputParameters.RequiredHeaders.Length 0 "should not have required headers for output"
            Expect.isFalse operation.OutputParameters.IsMultipart "should not have multipart output binding"
        }
    
        test "parse non-multipart operation" {
            let schema = ProducerDescription.Load(Uri(__SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"), "et", [])
            let services = schema.Services
            let operation = services.Head.Ports.Head.Methods |> List.find (fun op -> op.Name = "isikOtsing")
            Expect.equal operation.Name "isikOtsing" "wrong operation name"
            Expect.equal operation.Documentation (Some "Isiku andmete otsimine isikukoodi järgi") "wrong documentation"
            Expect.equal operation.Version (Some "v1") "wrong service version"
            Expect.equal operation.InputParameters.Parameters.Length 1 "wrong number of input parameters"
            Expect.equal operation.InputParameters.RequiredHeaders.Length 5 "wrong number of required headers in input"
            Expect.isFalse operation.InputParameters.IsMultipart "should not have multipart input binding"
            Expect.equal operation.OutputParameters.Parameters.Length 1 "wrong number of output parameters"
            Expect.equal operation.OutputParameters.RequiredHeaders.Length 5 "wrong number of required headers in output"
            Expect.isFalse operation.OutputParameters.IsMultipart "should not have multipart output binding"
        }
    
        test "parse Aktorstest xml schema definition" {
            let schema = ProducerDescription.Load(Uri(__SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"), "et", [])
            let typeSchemas = schema.TypeSchemas
            Expect.equal typeSchemas.Count 2 "wrong number of type schemas parsed"
            Expect.isTrue (typeSchemas.ContainsKey "http://aktorstest.x-road.ee/producer") "should contain aktorstest schema"
            Expect.isTrue (typeSchemas.ContainsKey "http://x-road.ee/xsd/x-road.xsd") "should contain x-road schema"
            let mainSchema = typeSchemas.["http://aktorstest.x-road.ee/producer"]
            Expect.equal mainSchema.TargetNamespace.NamespaceName "http://aktorstest.x-road.ee/producer" "wrong target namespace"
            Expect.isFalse mainSchema.QualifiedAttributes "should not qualify attributes"
            Expect.isFalse mainSchema.QualifiedElements "should not qualify elements"
            Expect.equal mainSchema.Elements.Count 18 "should define 18 schema elements"
            Expect.equal mainSchema.Types.Count 4 "should define 4 schema types"
        }
    ]
