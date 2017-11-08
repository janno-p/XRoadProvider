module XRoadProvider.Tests.SerializationTest

open Expecto
open System
open System.IO
open System.Text
open System.Threading
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.DynamicMethods
open XRoad.Serialization.Attributes
open XRoadProvider.Tests.SoapUtil

let [<Literal>] producerName = "producer"
let [<Literal>] producerNamespace = "http://producer.x-road.eu/"

module Types =
    type UnserializableType() =
        member val Value = Unchecked.defaultof<int> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithContent() =
        [<XRoadElement(MergeContent=true)>]
        member val ContentValue = Unchecked.defaultof<bool> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type ComplexType() =
        [<XRoadElement>]
        member val String = Unchecked.defaultof<string> with get, set
        [<XRoadElement>]
        member val BigInteger = Unchecked.defaultof<bigint> with get, set
    
    [<XRoadType(LayoutKind.Sequence)>]
    type SimpleType() =
        [<XRoadElement>]
        member val Value = Unchecked.defaultof<int> with get, set
        [<XRoadElement>]
        member val ComplexValue = Unchecked.defaultof<ComplexType> with get, set
        [<XRoadElement>]
        member val SubContent = Unchecked.defaultof<WithContent> with get, set
        member val IgnoredValue = true with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type WithNullableMembers() =
        [<XRoadElement(IsNullable=true)>]
        member val Value1 = Unchecked.defaultof<Nullable<int>> with get, set
        [<XRoadElement(IsNullable=true)>]
        member val Value2 = Unchecked.defaultof<Nullable<int>> with get, set

type Services =
    [<XRoadOperation("Service1", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", producerNamespace)>]
    [<XRoadResponse("Service1Response", producerNamespace)>]
    abstract Service1: unit -> unit

    [<XRoadOperation("Service2", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", producerNamespace)>]
    [<XRoadResponse("Service2Response", producerNamespace)>]
    abstract Service2: int64 -> string[]
    
    [<XRoadOperation("SimpleValueService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("SimpleValueService", producerNamespace)>]
    [<XRoadResponse("SimpleValueServiceResponse", producerNamespace)>]
    abstract SimpleValueService: [<XRoadParam("request")>] request: Types.SimpleType -> [<return: XRoadParam("response")>] Types.SimpleType
    
    [<XRoadOperation("StringService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("StringService", producerNamespace)>]
    [<XRoadResponse("StringServiceResponse", producerNamespace)>]
    abstract StringService: [<XRoadParam("request")>] request: string -> [<return: XRoadParam("response")>] string
    
    [<XRoadOperation("IntService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("IntService", producerNamespace)>]
    [<XRoadResponse("IntServiceResponse", producerNamespace)>]
    abstract IntService: [<XRoadParam("request")>] request: int32 -> [<return: XRoadParam("response")>] int32
    
    [<XRoadOperation("NullableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("NullableService", producerNamespace)>]
    [<XRoadResponse("NullableServiceResponse", producerNamespace)>]
    abstract NullableService: [<XRoadParam("request")>] request: Types.WithNullableMembers -> [<return: XRoadParam("response")>] Types.WithNullableMembers
    
    [<XRoadOperation("ComplexTypeService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ComplexTypeService", producerNamespace)>]
    [<XRoadResponse("ComplexTypeServiceResponse", producerNamespace)>]
    abstract ComplexTypeService: [<XRoadParam("request")>] request: Types.ComplexType -> [<return: XRoadParam("response")>] Types.ComplexType
    
    [<XRoadOperation("QualifiedRootService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("QualifiedRootService", producerNamespace)>]
    [<XRoadResponse("QualifiedRootServiceResponse", producerNamespace)>]
    abstract QualifiedRootService: [<XRoadParam("root", "urn:some-namespace")>] request: Types.SimpleType -> [<return: XRoadParam("root", "urn:some-namespace")>] Types.SimpleType
    
    [<XRoadOperation("UnserializableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("UnserializableService", producerNamespace)>]
    [<XRoadResponse("UnserializableServiceResponse", producerNamespace)>]
    abstract UnserializableService: [<XRoadParam("request")>] request: Types.UnserializableType -> [<return: XRoadParam("response")>] Types.UnserializableType

let deserialize (nm: string) (xml: string) =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use reader = XDocument.Parse(xml).CreateReader()
    map.Deserialize(reader, SerializerContext())

let serialize nm value =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use stream = new MemoryStream()
    use sw = new StreamWriter(stream, Encoding.UTF8)
    use writer = XmlWriter.Create(sw)
    writer.WriteStartDocument()
    map.Serialize(writer, SerializerContext(), value)
    writer.WriteEndDocument()
    writer.Flush()
    stream.Position <- 0L
    use reader = new StreamReader(stream, Encoding.UTF8)
    reader.ReadToEnd()

let simpleTypeEntity =
    let entity = Types.SimpleType(Value = 13)
    entity.ComplexValue <- Types.ComplexType(String = "test", BigInteger = 100I)
    entity.SubContent <- Types.WithContent(ContentValue = true)
    entity

let [<Tests>] tests =
    testList "serialization tests" [
        ptest "can handle array type response" {
            let xml = @"<response><item /><item /></response>"
            let response = xml |> deserialize "Service2" |> unbox<string[]>
            Expect.isNotNull response "response should have value"
            Expect.equal response.Length 2 "response should have exactly 2 items"
        }
        
        test "can serialize unit request" {
            let xml = serialize "Service1" [||]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Service1 xmlns=""http://producer.x-road.eu/"" />" "invalid xml result"
        }
        
        test "can deserialize unit response" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><Service1 xmlns=""http://producer.x-road.eu/"" />"
            let response = xml |> deserialize "Service1"
            Expect.equal response [||] "invalid xml result"
        }
        
        test "can serialize simple value" {
            let xml = serialize "SimpleValueService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><SimpleValueService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></request></SimpleValueService>" "invalid xml result"
        }
        
        ptest "can deserialize simple value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><SimpleValueServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns=""""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></response></SimpleValueServiceResponse>"
            let response = xml |> deserialize "SimpleValueService"
            Expect.equal 1 response.Length "should return exactly 1 value"
            Expect.isTrue (response.[0] :? Types.SimpleType) "wrong return type"
            let result: Types.SimpleType = response.[0] |> unbox
            Expect.equal result.Value 13 "wrong result.Value value"
            Expect.equal result.ComplexValue.BigInteger 100I "wrong result.ComplexValue.BigInteger value"
            Expect.equal result.ComplexValue.String "test" "wrong result.ComplexValue.String value"
            Expect.isTrue result.SubContent.ContentValue "wrong result.SubContent.ContentValue value"
        }
        
        test "serialize null value" {
            let xml = serialize "StringService" [| (null: string) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><StringService xmlns=""http://producer.x-road.eu/""><request p2:nil=""true"" xmlns:p2=""http://www.w3.org/2001/XMLSchema-instance"" xmlns="""" /></StringService>" "invalid serialization result"
        }
        
        ptest "deserialize null value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xsi:nil=""true"" /></StringServiceResponse>"
            let response = xml |> deserialize "StringService"
            Expect.isNull response "response should be null"
        }
        
        test "write qualified root name" {
            let xml = serialize "QualifiedRootService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><QualifiedRootService xmlns=""http://producer.x-road.eu/""><root xmlns=""urn:some-namespace""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></root></QualifiedRootService>" "invalid xml result"
        }
        
        test "serializing unserializable type" {
            Expect.throwsC
                (fun _ ->
                    let value = Types.UnserializableType(Value = 10)
                    serialize "UnserializableService" [| value |] |> ignore)
                (fun e -> Expect.equal e.Message "Type `XRoadProvider.Tests.SerializationTest+Types+UnserializableType` is not serializable." "invalid exception")
        }
        
        test "serialize string value" {
            let xml = serialize "StringService" [| "string value" |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><StringService xmlns=""http://producer.x-road.eu/""><request xmlns="""">string value</request></StringService>" "invalid serialization result"
        }
        
        ptest "deserialize string value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns="""">string value</response></StringServiceResponse>"
            let response = xml |> deserialize "StringService" |> unbox<string>
            Expect.equal response "string value" "response not equal to 'string value'"
        }
        
        test "serialize int value" {
            let xml = serialize "IntService" [| 32 |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><IntService xmlns=""http://producer.x-road.eu/""><request xmlns="""">32</request></IntService>" "invalid serialization result"
        }
        
        ptest "deserialize int value" {
            failtest "needs review"
            (*
            let resultXml = 32 |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>32</keha></wrapper>"
            resultXml |> deserialize'<int> |> should equal 32
            *)
        }
        
        test "serialize nullable values" {
            let xml = serialize "NullableService" [| Types.WithNullableMembers(Value1 = Nullable(13)) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><NullableService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value1>13</Value1><Value2 p3:nil=""true"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance"" /></request></NullableService>" "invalid serialization result"
        }
        
        ptest "deserialize nullable values" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithNullableMembers>
            result |> should not' (be Null)
            result.Value1 |> should not' (be Null)
            result.Value1 |> should equal 13
            result.Value2 |> should be Null
            *)
        }
        
        test "serialize not nullable as null" {
            Expect.throwsC
                (fun _ -> serialize "ComplexTypeService" [| Types.ComplexType(String = null) |] |> ignore)
                (fun e ->
                    let e = e :?> System.Reflection.TargetInvocationException
                    Expect.equal e.InnerException.Message "Not nullable property `String` of type `ComplexType` has null value." "invalid exception message")
        }
        
        ptest "serialize choice with abstract root element" {
            failtest "needs review"
            (*
            let optionEntity = TestType.Concrete1(SubValue1 = "test2", BaseValue = "test")
            let resultXml = TestType.TypeWithAbstractChoice(X = TestType.AbstractRootChoice.New_value1(optionEntity)) |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><value1 xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></value1></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.TypeWithAbstractChoice>
            result |> should not' (be Null)
            result.X |> should not' (be Null)
            let (success, value) = result.X.TryGet_value1()
            success |> should equal true
            value |> should not' (be Null)
            value.BaseValue |> should equal "test"
            *)
        }
        
        ptest "serialize array with default property names" {
            failtest "needs review"
            (*
            let entity = TestType.WithArray1(Array = [| true; false; true; true |])
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Array><item>true</item><item>false</item><item>true</item><item>true</item></Array></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithArray1>
            result |> should not' (be Null)
            result.Array |> should not' (be Null)
            result.Array |> should equal entity.Array
            *)
        }
        
        ptest "deserialize abstract type" {
            failtest "needs review"
            (*
            (fun () ->
                @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></keha></wrapper>"
                |> deserialize'<TestType.AbstractBase>
                |> ignore)
            |> should (throwWithMessage "Cannot deserialize abstract type `AbstractBase`.") typeof<Exception>
            *)
        }
        
        ptest "deserialize abstract type with no sub types" {
            failtest "needs review"
            (*
            (fun () ->
                @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></keha></wrapper>"
                |> deserialize'<TestType.AbstractBaseWithNoSubTypes>
                |> ignore)
            |> should (throwWithMessage "Cannot deserialize abstract type `AbstractBaseWithNoSubTypes`.") typeof<Exception>
            *)
        }
        
        ptest "serialize extended type with base type contents" {
            failtest "needs review"
            (*
            let entity = TestType.ExtendedType(OwnElement = "test", String = "test", BigInteger = 100I)
            let resultXml = entity |> serializeWithContext<TestType.ComplexType> (XmlQualifiedName("keha")) [] (SerializerContext())
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.ExtendedType>
            result |> should not' (be Null)
            result.BigInteger |> should equal entity.BigInteger
            result.OwnElement |> should equal entity.OwnElement
            result.String |> should equal entity.String
            let result = resultXml |> deserialize'<TestType.ComplexType>
            result |> should not' (be Null)
            result |> should be instanceOfType<TestType.ExtendedType>
            (result :?> TestType.ExtendedType).OwnElement |> should equal entity.OwnElement
            result.BigInteger |> should equal entity.BigInteger
            result.String |> should equal entity.String
            *)
        }
        
        ptest "serialize base type when subtype is used" {
            failtest "needs review"
            (*
            let entityMember = TestType.ExtendedType(OwnElement = "test", String = "test", BigInteger = 100I)
            let entity = TestType.UseBaseClass(Member = entityMember)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Member xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.UseBaseClass>
            result |> should not' (be Null)
            result.Member |> should not' (be Null)
            result.Member |> should be instanceOfType<TestType.ExtendedType>
            result.Member.BigInteger |> should equal entityMember.BigInteger
            (result.Member :?> TestType.ExtendedType).OwnElement |> should equal entityMember.OwnElement
            result.Member.String |> should equal entityMember.String
            *)
        }
        
        ptest "serialize abstract base type when subtype is used" {
            failtest "needs review"
            (*
            let concreteEntity = TestType.Concrete1(SubValue1 = "test2", BaseValue = "test")
            let entity = TestType.Referrer(Reference = concreteEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Reference xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.Referrer>
            result |> should not' (be Null)
            result.Reference |> should not' (be Null)
            result.Reference |> should be instanceOfType<TestType.Concrete1>
            (result.Reference :?> TestType.Concrete1).SubValue1 |> should equal concreteEntity.SubValue1
            *)
        }
        
        ptest "serialize abstract base type when subtype is used (with explicit name and namespace)" {
            failtest "needs review"
            (*
            let concreteEntity = TestType.Concrete3(SubValue3 = "test2", BaseValue = "test")
            let entity = TestType.Referrer(Reference = concreteEntity)
            let resultXml = entity |> serializeWithContext (XmlQualifiedName("keha")) ["t", "testns"] (SerializerContext())
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:t=""testns""><keha><Reference xsi:type=""t:ConcreteTypeName""><BaseValue>test</BaseValue><SubValue3>test2</SubValue3></Reference></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.Referrer>
            result |> should not' (be Null)
            result.Reference |> should not' (be Null)
            result.Reference |> should be instanceOfType<TestType.Concrete3>
            (result.Reference :?> TestType.Concrete3).SubValue3 |> should equal concreteEntity.SubValue3
            *)
        }
        
        ptest "serialize inner abstract base type" {
            failtest "needs review"
            (*
            let referenceEntity = TestType.Concrete1(SubValue1 = "kino", BaseValue = "basev")
            let referrerEntity = TestType.Referrer(Reference = referenceEntity)
            let entity = TestType.InnerReferrer(Ref = referrerEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Ref><Reference xsi:type=""Concrete1""><BaseValue>basev</BaseValue><SubValue1>kino</SubValue1></Reference></Ref></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.InnerReferrer>
            result |> should not' (be Null)
            result.Ref |> should not' (be Null)
            result.Ref.Reference |> should not' (be Null)
            result.Ref.Reference |> should be instanceOfType<TestType.Concrete1>
            (result.Ref.Reference :?> TestType.Concrete1).SubValue1 |> should equal referenceEntity.SubValue1
            result.Ref.Reference.BaseValue |> should equal referenceEntity.BaseValue
            *)
        }
        
        ptest "serialize choice type 1" {
            failtest "needs review"
            (*
            let optionEntity = TestType.Choice1(Choice1Element = "test")
            let entity = TestType.TestChoice.NewChoice1(optionEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Choice1Element>test</Choice1Element></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.TestChoice>
            result |> should not' (be Null)
            let (success, value) = result.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice1Element |> should equal optionEntity.Choice1Element
            let (success, value) = result.TryGetChoice2()
            success |> should equal false
            value |> should be Null
            *)
        }
        
        ptest "serialize choice type 2" {
            failtest "needs review"
            (*
            let optionEntity = TestType.Choice2(Choice2Element = "test")
            let entity = TestType.TestChoice.NewChoice2(optionEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Choice2><Choice2Element>test</Choice2Element></Choice2></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.TestChoice>
            result |> should not' (be Null)
            let (success, value) = result.TryGetChoice1()
            success |> should equal false
            value |> should be Null
            let (success, value) = result.TryGetChoice2()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice2Element |> should equal optionEntity.Choice2Element
            *)
        }
        
        ptest "serialize inner choice 1 element" {
            failtest "needs review"
            (*
            let optionEntity = TestType.Choice1(Choice1Element = "test")
            let choiceEntity = TestType.TestChoice.NewChoice1(optionEntity)
            let entity = TestType.WithChoice(NotAChoice = "tere", IsAChoice = choiceEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><NotAChoice>tere</NotAChoice><Choice1Element>test</Choice1Element></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should equal entity.NotAChoice
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice1Element |> should equal optionEntity.Choice1Element
            let (success, value) = result.IsAChoice.TryGetChoice2()
            success |> should equal false
            value |> should be Null
            *)
        }
        
        ptest "serialize inner choice 2 element" {
            failtest "needs review"
            (*
            let optionEntity = TestType.Choice2(Choice2Element = "test")
            let choiceEntity = TestType.TestChoice.NewChoice2(optionEntity)
            let entity = TestType.WithChoice(NotAChoice = "tere", IsAChoice = choiceEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><NotAChoice>tere</NotAChoice><Choice2><Choice2Element>test</Choice2Element></Choice2></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should equal entity.NotAChoice
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal false
            value |> should be Null
            let (success, value) = result.IsAChoice.TryGetChoice2()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice2Element |> should equal optionEntity.Choice2Element
            *)
        }
        
        ptest "serialize empty string" {
            failtest "needs review"
            (*
            let optionEntity = TestType.Choice1(Choice1Element = "test")
            let choiceEntity = TestType.TestChoice.NewChoice1(optionEntity)
            let entity = TestType.WithChoice(NotAChoice = "", IsAChoice = choiceEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><NotAChoice /><Choice1Element>test</Choice1Element></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should equal ""
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            *)
        }
        
        ptest "deserialize null string" {
            failtest "needs review"
            (*
            let optionEntity = TestType.Choice1(Choice1Element = "test")
            let choiceEntity = TestType.TestChoice.NewChoice1(optionEntity)
            let entity = TestType.WithChoice(NotAChoice = null, IsAChoice = choiceEntity)
            let resultXml = entity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><NotAChoice xsi:nil=""true"" /><Choice1Element>test</Choice1Element></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should be Null
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            *)
        }
        
        ptest "serialize null array" {
            failtest "needs review"
            (*
            let resultXml = TestType.WithArray1() |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Array xsi:nil=""true"" /></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithArray1>
            result |> should not' (be Null)
            result.Array |> should be Null
            *)
        }
        
        ptest "serialize inline file" {
            failtest "needs review"
            (*
            let context = SerializerContext()
            let entity = TestType.WithBinaryContent(BinaryContent=BinaryContent.Create([| 1uy; 2uy; 3uy; 4uy |]))
            let resultXml = entity |> serializeWithContext' context
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BinaryContent>AQIDBA==</BinaryContent></keha></wrapper>"
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 0
            let result = resultXml |> deserializeWithContext'<TestType.WithBinaryContent> context
            result |> should not' (be Null)
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 0
            result.BinaryContent |> should not' (be Null)
            result.BinaryContent.ContentID |> should not' (be Null)
            result.BinaryContent.GetBytes() |> should equal [| 1uy; 2uy; 3uy; 4uy |]
            *)
        }
        
        ptest "serialize multipart file" {
            failtest "needs review"
            (*
            let context = SerializerContext(IsMultipart=true)
            let entity = TestType.WithBinaryContent(BinaryContent=BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let resultXml = entity |> serializeWithContext' context
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BinaryContent href=""cid:Content-ID"" /></keha></wrapper>"
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 1
            context.Attachments.ContainsKey("Content-ID") |> should equal true
            let result = resultXml |> deserializeWithContext'<TestType.WithBinaryContent> context
            result |> should not' (be Null)
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 1
            context.Attachments.ContainsKey("Content-ID") |> should equal true
            result.BinaryContent |> should not' (be Null)
            result.BinaryContent.ContentID |> should equal "Content-ID"
            result.BinaryContent |> should be (sameAs context.Attachments.["Content-ID"])
            result.BinaryContent.GetBytes() |> should equal [| 1uy; 2uy; 3uy; 4uy |]
            *)
        }
        
        ptest "serialize xop file" {
            failtest "needs review"
            (*
            let context = SerializerContext()
            let entity = TestType.WithXopBinaryContent(BinaryContent=BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let resultXml = entity |> serializeWithContext' context
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BinaryContent><xop:Include href=""cid:Content-ID"" xmlns:xop=""http://www.w3.org/2004/08/xop/include"" /></BinaryContent></keha></wrapper>"
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 1
            context.Attachments.ContainsKey("Content-ID") |> should equal true
            let result = resultXml |> deserializeWithContext'<TestType.WithXopBinaryContent> context
            result |> should not' (be Null)
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 1
            context.Attachments.ContainsKey("Content-ID") |> should equal true
            result.BinaryContent |> should not' (be Null)
            result.BinaryContent.ContentID |> should equal "Content-ID"
            result.BinaryContent |> should be (sameAs context.Attachments.["Content-ID"])
            result.BinaryContent.GetBytes() |> should equal [| 1uy; 2uy; 3uy; 4uy |]
            *)
        }
        
        ptest "can serialize type with optional reference type members" {
            failtest "needs review"
            (*
            let resultXml = TestType.HasOptionalElements(Value1 = Optional.Option.Some<string>("value")) |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value1>value</Value1></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be True
            result.Value1.ValueOr("") |> should equal "value"
            result.Value2.HasValue |> should be False
            result.Array1.HasValue |> should be False
            *)
        }
        
        ptest "can serialize type with optional value type members" {
            failtest "needs review"
            (*
            let resultXml = TestType.HasOptionalElements(Value2 = Optional.Option.Some<int32>(15)) |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value2>15</Value2></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be False
            result.Value2.HasValue |> should be True
            result.Value2.ValueOr(0) |> should equal 15
            result.Array1.HasValue |> should be False
            *)
        }
        
        ptest "can serialize type with optional array type members" {
            failtest "needs review"
            (*
            let resultXml = TestType.HasOptionalElements(Array1 = Optional.Option.Some<int[]>([| 1; 2; 3 |])) |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Array1><item>1</item><item>2</item><item>3</item></Array1></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be False
            result.Value2.HasValue |> should be False
            result.Array1.HasValue |> should be True
            result.Array1.ValueOr(null: int[]) |> should equal [| 1; 2; 3 |]
            *)
        }
        
        ptest "can serialize type with no optional members set" {
            failtest "needs review"
            (*
            let resultXml = TestType.HasOptionalElements() |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha /></wrapper>"
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be False
            result.Value2.HasValue |> should be False
            *)
        }
        
        ptest "serialize array of system type values" {
            failtest "needs review"
            (*
            let resultXml = [| "1"; "2"; "3" |] |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><item>1</item><item>2</item><item>3</item></keha></wrapper>"
            resultXml |> deserialize'<string> |> should equal [| "1"; "2"; "3" |]
            *)
        }
        
        ptest "serialize root optional some value" {
            failtest "needs review"
            (*
            let initial = Optional.Option.Some<int>(202)
            let resultXml = initial |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>202</keha></wrapper>"
            resultXml |> deserialize'<Optional.Option<int>> |> should equal initial
            *)
        }
        
        ptest "serialize root optional none value" {
            failtest "needs review"
            (*
            let initial = Optional.Option.None<int>()
            let resultXml = initial |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" />"
            resultXml |> deserialize'<Optional.Option<int>> |> should equal initial
            *)
        }
        
        ptest "serialize multiple levels of inheritance" {
            failtest "needs review"
            (*
            let initial = Level3(Value1 = Nullable<int>(1), Value2 = Nullable<int>(2), Value3 = Nullable<int>(3))
            let resultXml = initial |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value1>1</Value1><Value2>2</Value2><Value3>3</Value3></keha></wrapper>"
            resultXml |> deserialize'<Level3> |> should equal initial
            *)
        }
    ]
