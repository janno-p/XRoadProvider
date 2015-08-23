module XRoadSerializer.Tests.Serialization

open FsUnit
open NUnit.Framework
open System.IO
open System.Text
open System.Xml
open XRoad

module TestType =
    type SimpleType() =
        member val Value = 13 with get, set

let [<Test>] ``initializes new serializer`` () =
    let serializer = Serializer()
    serializer |> should not' (equal null)

let serialize value =
    let serializer = Serializer()
    let sb = StringBuilder()
    use sw = new StringWriter(sb)
    use writer = XmlWriter.Create(sw)
    serializer.Serialize(writer, value, XmlQualifiedName("keha"))
    writer.Flush()
    sw.Flush()
    sb.ToString()

let [<Test>] ``can serialize simple value`` () =
    let result = TestType.SimpleType() |> serialize
    result |> should be (equal @"<?xml version=""1.0"" encoding=""utf-16""?><keha />")
