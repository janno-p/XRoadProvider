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

    [<XRoadType(LayoutKind.Sequence)>]
    type WithContent() =
        [<XRoadContent>]
        member val ContentValue = true with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type ComplexType() =
        [<XRoadElement>]
        member val String = "test" with get, set
        [<XRoadElement>]
        member val BigInteger = 100I with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type SimpleType() =
        [<XRoadElement>]
        member val Value = 13 with get, set
        [<XRoadElement>]
        member val ComplexValue = ComplexType() with get, set
        [<XRoadElement>]
        member val SubContent = WithContent() with get, set
        member val IgnoredValue = true with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type WithNullableMembers() =
        [<XRoadElement(IsNullable=true)>]
        member val Value1 = Nullable(13) with get, set
        [<XRoadElement(IsNullable=true)>]
        member val Value2 = Nullable() with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type ExtendedType() =
        inherit ComplexType()
        [<XRoadElement>]
        member val OwnElement = "test" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type UseBaseClass() =
        [<XRoadElement>]
        member val Member = ExtendedType() :> ComplexType with get, set

    [<AbstractClass; XRoadType(LayoutKind.Sequence)>]
    type AbstractBase() =
        [<XRoadElement>]
        member val BaseValue = "test" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete1() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue1 = "test2" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete2() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue2 = "test3" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete3() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue3 = "test2" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type Referrer() =
        [<XRoadElement>]
        member val Reference = Concrete1() :> AbstractBase with get, set

let serialize qn value =
    let serializer = Serializer()
    use stream = new MemoryStream()
    use sw = new StreamWriter(stream, Encoding.UTF8)
    use writer = XmlWriter.Create(sw)
    writer.WriteStartDocument()
    writer.WriteStartElement("wrapper")
    writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
    serializer.Serialize(writer, value, qn)
    writer.WriteEndElement()
    writer.WriteEndDocument()
    writer.Flush()
    sw.Flush()
    stream.Position <- 0L
    use sr = new StreamReader(stream, Encoding.UTF8)
    sr.ReadToEnd()

let serialize' v = serialize (XmlQualifiedName("keha")) v

let [<Test>] ``initializes new serializer`` () =
    let serializer = Serializer()
    serializer |> should not' (equal null)

let [<Test>] ``can serialize simple value`` () =
    let result = TestType.SimpleType() |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></keha></wrapper>"

let [<Test>] ``serialize null value`` () =
    let result = (null: string) |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha xsi:nil=""true"" /></wrapper>"

let [<Test>] ``write qualified root name`` () =
    let result = TestType.SimpleType() |> serialize (XmlQualifiedName("root", "urn:some-namespace"))
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><root xmlns=""urn:some-namespace""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></root></wrapper>"

let [<Test>] ``serializing unserializable type`` () =
    TestDelegate(fun _ -> TestType.UnserializableType() |> serialize' |> ignore)
    |> should (throwWithMessage "Type `XRoadSerializer.Tests.Serialization+TestType+UnserializableType` is not serializable.") typeof<Exception>

let [<Test>] ``serialize string value`` () =
    "string value" |> serialize' |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>string value</keha></wrapper>"

let [<Test>] ``serialize integer value`` () =
    32 |> serialize' |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>32</keha></wrapper>"

let [<Test>] ``serialize nullable values`` () =
    let result = TestType.WithNullableMembers() |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value1>13</Value1><Value2 xsi:nil=""true"" /></keha></wrapper>"

let [<Test>] ``serialize not nullable as null`` () =
    TestDelegate (fun _ -> TestType.ComplexType(String = null) |> serialize' |> ignore)
    |> should (throwWithMessage "Not nullable property `String` of type `XRoadSerializer.Tests.Serialization+TestType+ComplexType` has null value.") typeof<Exception>

let [<Test>] ``serialize extended type with base type contents`` () =
    let result = TestType.ExtendedType() |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></keha></wrapper>"

let [<Test>] ``serialize base type when subtype is used`` () =
    let result = TestType.UseBaseClass() |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Member xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></keha></wrapper>"

let [<Test>] ``serialize abstract base type when subtype is used`` () =
    let result = TestType.Referrer() |> serialize'
    result |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Reference xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></keha></wrapper>"
