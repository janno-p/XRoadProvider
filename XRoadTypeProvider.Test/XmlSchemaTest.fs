namespace XRoadTypeProvider.Test

open NUnit.Framework
open Swensen.Unquote
open System.Xml.Linq
open XRoadTypeProvider.Wsdl.XsdSchema

[<TestFixture>]
module XmlSchemaTest =
    [<Test>]
    let ``Header element inside schema after type definitions`` () =
        let fragment = "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\">\
                           <element name=\"testElement\" type=\"string\" />\
                           <annotation />\
                           <include namespace=\"urn:some-namespace-to-include\" />\
                        </schema>"
        let schemaNode = XElement.Parse(fragment)
        raisesWith<exn> <@ parseSchemaNode(schemaNode) @> (fun e -> <@ e.Message = "Element {http://www.w3.org/2001/XMLSchema}include inside schema element was not expected at the current position!" @>)
