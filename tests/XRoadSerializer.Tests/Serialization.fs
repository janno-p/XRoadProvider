module XRoadSerializer.Tests.Serialization

open FsUnit
open NUnit.Framework
open System
open System.IO
open System.Text
open System.Xml
open XRoad
open XRoad.Attributes

module TestType =
    type UnserializableType() =
        member val Value = 10 with get, set

    [<XRoadType>]
    type SimpleType() =
        member val Value = 13 with get, set

let serialize qn value =
    let serializer = Serializer()
    let sb = StringBuilder()
    use sw = new StringWriter(sb)
    use writer = XmlWriter.Create(sw)
    serializer.Serialize(writer, value, qn)
    writer.Flush()
    sw.Flush()
    sb.ToString()

let serialize' v = serialize (XmlQualifiedName("keha")) v

let [<Test>] ``initializes new serializer`` () =
    let serializer = Serializer()
    serializer |> should not' (equal null)

let [<Test>] ``can serialize simple value`` () =
    let result = TestType.SimpleType() |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-16""?><keha />"

let [<Test>] ``serialize null value`` () =
    let result = (null: string) |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-16""?><keha p1:nil=""true"" xmlns:p1=""http://www.w3.org/2001/XMLSchema-instance"" />"

let [<Test>] ``write qualified root name`` () =
    let result = TestType.SimpleType() |> serialize (XmlQualifiedName("root", "urn:some-namespace"))
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-16""?><root xmlns=""urn:some-namespace"" />"

let [<Test>] ``serializing unserializable type`` () =
    TestDelegate(fun _ -> TestType.UnserializableType() |> serialize' |> ignore)
    |> should (throwWithMessage "Type `XRoadSerializer.Tests.Serialization+TestType+UnserializableType` is not serializable.") typeof<Exception>

let [<Test>] ``serialize string value`` () =
    "string value" |> serialize' |> should equal @"<?xml version=""1.0"" encoding=""utf-16""?><keha>string value</keha>"

let [<Test>] ``serialize integer value`` () =
    32 |> serialize' |> should equal @"<?xml version=""1.0"" encoding=""utf-16""?><keha>32</keha>"
