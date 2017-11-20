module XRoadProvider.Tests.SerializationTest

open Expecto
open System
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Threading
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.Emitter
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
    type ExtendedType() =
        inherit ComplexType()
        [<XRoadElement>]
        member val OwnElement = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type UseBaseClass() =
        [<XRoadElement>]
        member val Member = Unchecked.defaultof<ComplexType> with get, set
    
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
        
    [<AbstractClass; AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type AbstractBase() =
        [<XRoadElement>]
        member val BaseValue = Unchecked.defaultof<string> with get, set
        
    [<AbstractClass; AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type AbstractBaseWithNoSubTypes() =
        [<XRoadElement>]
        member val BaseValue = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type Referrer() =
        [<XRoadElement>]
        member val Reference = Unchecked.defaultof<AbstractBase> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete1() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue1 = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete2() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue2 = Unchecked.defaultof<string> with get, set

    [<XRoadType("ConcreteTypeName", LayoutKind.Sequence, Namespace="testns")>]
    type Concrete3() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue3 = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Choice)>]
    [<XRoadChoiceOption(1, "value1", MergeContent=false)>]
    [<XRoadChoiceOption(2, "value2", MergeContent=false)>]
    type AbstractRootChoice =
        val private __id: int
        val private __value: obj
        private new(id, value: obj) = { __id = id; __value = value }
        member this.TryGet_value1([<Out>] value: AbstractBase byref) =
            if this.__id = 1 then value <- unbox this.__value
            else value <- null
            this.__id = 1
        member this.TryGet_value2([<Out>] value: string byref) =
            if this.__id = 2 then value <- unbox this.__value
            else value <- null
            this.__id = 2
        static member New_value1(value: AbstractBase) = AbstractRootChoice(1, value)
        static member New_value2(value: string) = AbstractRootChoice(2, value)
        
    [<XRoadType(LayoutKind.Sequence)>]
    type TypeWithAbstractChoice() =
        [<XRoadElement>]
        member val X = Unchecked.defaultof<AbstractRootChoice> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithArray1() =
        [<XRoadElement(IsNullable=true)>]
        [<XRoadCollection(ItemIsNullable=true)>]
        member val Array = Unchecked.defaultof<bool[]> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type InnerReferrer() =
        [<XRoadElement>]
        member val Ref = Unchecked.defaultof<Referrer> with get, set
        
    [<AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type Choice1() =
        [<XRoadElement>]
        member val Choice1Element = Unchecked.defaultof<string> with get, set

    [<AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type Choice2() =
        [<XRoadElement>]
        member val Choice2Element = Unchecked.defaultof<string> with get, set

    [<XRoadType(LayoutKind.Choice)>]
    [<XRoadChoiceOption(1, "Choice1", MergeContent=true)>]
    [<XRoadChoiceOption(2, "Choice2", MergeContent=false)>]
    type TestChoice =
        val private __id: int
        val private __value: obj
        private new(id, value: obj) = { __id = id; __value = value }
        member this.TryGetChoice1([<Out>] value: Choice1 byref) =
            if this.__id = 1 then value <- unbox this.__value
            else value <- null
            this.__id = 1
        member this.TryGetChoice2([<Out>] value: Choice2 byref) =
            if this.__id = 2 then value <- unbox this.__value
            else value <- null
            this.__id = 2
        static member NewChoice1(value: Choice1) = TestChoice(1, value)
        static member NewChoice2(value: Choice2) = TestChoice(2, value)
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithChoice() =
        [<XRoadElement(IsNullable=true)>]
        member val NotAChoice = Unchecked.defaultof<string> with get, set
        [<XRoadElement>]
        member val IsAChoice = Unchecked.defaultof<TestChoice> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithBinaryContent() =
        [<XRoadElement>]
        member val BinaryContent = Unchecked.defaultof<BinaryContent> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithXopBinaryContent() =
        [<XRoadElement(UseXop=true)>]
        member val BinaryContent = Unchecked.defaultof<BinaryContent> with get, set
        
    [<XRoadType>]
    type HasOptionalElements () =
        [<XRoadElement>]
        member val Value1 = Optional.Option.None<string>() with get, set
        [<XRoadElement>]
        member val Value2 = Optional.Option.None<int>() with get, set
        [<XRoadElement; XRoadCollection("item")>]
        member val Array1 = Optional.Option.None<int[]>() with get, set
        
    [<XRoadType>]
    type Level1 () =
        [<XRoadElement>]
        member val Value1 = Nullable<int>() with get, set
    
    [<XRoadType>]
    type Level2 () =
        inherit Level1()
        [<XRoadElement>]
        member val Value2 = Nullable<int>() with get, set
    
    [<XRoadType>]
    type Level3 () =
        inherit Level2()
        [<XRoadElement>]
        member val Value3 = Nullable<int>() with get, set

module ResultTypes =
    type [<XRoadType>] Service1Result () =
        class end
    
    type [<XRoadType>] Service2Result () =
        [<XRoadElement>] member val response = Unchecked.defaultof<string[]> with get, set
        
    type [<XRoadType>] SimpleValueServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.SimpleType> with get, set
        
    type [<XRoadType>] StringServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<string> with get, set

    type [<XRoadType>] IntServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<int32> with get, set
        
    type [<XRoadType>] NullableServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.WithNullableMembers> with get, set
        
    type [<XRoadType>] ComplexTypeServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.ComplexType> with get, set
        
    type [<XRoadType>] QualifiedRootServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.SimpleType> with get, set
        
    type [<XRoadType>] UnserializableServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.UnserializableType> with get, set
        
    type [<XRoadType>] AbstractChoiceServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.TypeWithAbstractChoice> with get, set
        
    type [<XRoadType>] WithArray1ServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.WithArray1> with get, set
        
    type [<XRoadType>] UseBaseClassServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.UseBaseClass> with get, set
        
    type [<XRoadType>] ReferrerServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.Referrer> with get, set
        
    type [<XRoadType>] InnerReferrerServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.InnerReferrer> with get, set
        
    type [<XRoadType>] TestChoiceServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.TestChoice> with get, set
        
    type [<XRoadType>] WithChoiceServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.WithChoice> with get, set
        
    type [<XRoadType>] WithBinaryContentServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.WithBinaryContent> with get, set
        
    type [<XRoadType>] WithXopBinaryContentServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.WithXopBinaryContent> with get, set
        
    type [<XRoadType>] HasOptionalElementsServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.HasOptionalElements> with get, set
        
    type [<XRoadType>] OptionalIntServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Optional.Option<int>> with get, set
        
    type [<XRoadType>] Level3ServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.Level3> with get, set
        
    type [<XRoadType>] AbstractBaseServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.AbstractBase> with get, set
        
    type [<XRoadType>] AbstractBaseWithNoSubTypesServiceResult () =
        [<XRoadElement>] member val response = Unchecked.defaultof<Types.AbstractBaseWithNoSubTypes> with get, set

type Services =
    [<XRoadOperation("Service1", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", producerNamespace)>]
    [<XRoadResponse("Service1Response", producerNamespace)>]
    abstract Service1: unit -> ResultTypes.Service1Result

    [<XRoadOperation("Service2", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", producerNamespace)>]
    [<XRoadResponse("Service2Response", producerNamespace)>]
    abstract Service2: int64 -> ResultTypes.Service2Result
    
    [<XRoadOperation("SimpleValueService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("SimpleValueService", producerNamespace)>]
    [<XRoadResponse("SimpleValueServiceResponse", producerNamespace)>]
    abstract SimpleValueService: [<XRoadElement("request")>] request: Types.SimpleType -> ResultTypes.SimpleValueServiceResult
    
    [<XRoadOperation("StringService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("StringService", producerNamespace)>]
    [<XRoadResponse("StringServiceResponse", producerNamespace)>]
    abstract StringService: [<XRoadElement("request", IsNullable = true)>] request: string -> ResultTypes.StringServiceResult
    
    [<XRoadOperation("IntService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("IntService", producerNamespace)>]
    [<XRoadResponse("IntServiceResponse", producerNamespace)>]
    abstract IntService: [<XRoadElement("request")>] request: int32 -> ResultTypes.IntServiceResult
    
    [<XRoadOperation("NullableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("NullableService", producerNamespace)>]
    [<XRoadResponse("NullableServiceResponse", producerNamespace)>]
    abstract NullableService: [<XRoadElement("request")>] request: Types.WithNullableMembers -> ResultTypes.NullableServiceResult
    
    [<XRoadOperation("ComplexTypeService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ComplexTypeService", producerNamespace)>]
    [<XRoadResponse("ComplexTypeServiceResponse", producerNamespace)>]
    abstract ComplexTypeService: [<XRoadElement("request")>] request: Types.ComplexType -> ResultTypes.ComplexTypeServiceResult
    
    [<XRoadOperation("QualifiedRootService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("QualifiedRootService", producerNamespace)>]
    [<XRoadResponse("QualifiedRootServiceResponse", producerNamespace)>]
    abstract QualifiedRootService: [<XRoadElement("root", Namespace = "urn:some-namespace")>] request: Types.SimpleType -> ResultTypes.QualifiedRootServiceResult
    
    [<XRoadOperation("UnserializableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("UnserializableService", producerNamespace)>]
    [<XRoadResponse("UnserializableServiceResponse", producerNamespace)>]
    abstract UnserializableService: [<XRoadElement("request")>] request: Types.UnserializableType -> ResultTypes.UnserializableServiceResult

    [<XRoadOperation("AbstractChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("AbstractChoiceService", producerNamespace)>]
    [<XRoadResponse("AbstractChoiceServiceResponse", producerNamespace)>]
    abstract AbstractChoiceService: [<XRoadElement("request")>] request: Types.TypeWithAbstractChoice -> ResultTypes.AbstractChoiceServiceResult
    
    [<XRoadOperation("WithArray1Service", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithArray1Service", producerNamespace)>]
    [<XRoadResponse("WithArray1ServiceResponse", producerNamespace)>]
    abstract WithArray1Service: [<XRoadElement("request")>] request: Types.WithArray1 -> ResultTypes.WithArray1ServiceResult
    
    [<XRoadOperation("ExtendedTypeService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ExtendedTypeService", producerNamespace)>]
    [<XRoadResponse("ExtendedTypeServiceResponse", producerNamespace)>]
    abstract ExtendedTypeService: [<XRoadElement("request")>] request: Types.ComplexType -> ResultTypes.ComplexTypeServiceResult
    
    [<XRoadOperation("UseBaseClassService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("UseBaseClassService", producerNamespace)>]
    [<XRoadResponse("UseBaseClassServiceResponse", producerNamespace)>]
    abstract UseBaseClassService: [<XRoadElement("request")>] request: Types.UseBaseClass -> ResultTypes.UseBaseClassServiceResult
    
    [<XRoadOperation("ReferrerService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ReferrerService", producerNamespace)>]
    [<XRoadResponse("ReferrerServiceResponse", producerNamespace)>]
    abstract ReferrerService: [<XRoadElement("request")>] request: Types.Referrer -> ResultTypes.ReferrerServiceResult
    
    [<XRoadOperation("InnerReferrerService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("InnerReferrerService", producerNamespace)>]
    [<XRoadResponse("InnerReferrerServiceResponse", producerNamespace)>]
    abstract InnerReferrerService: [<XRoadElement("request")>] request: Types.InnerReferrer -> ResultTypes.InnerReferrerServiceResult
    
    [<XRoadOperation("TestChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("TestChoiceService", producerNamespace)>]
    [<XRoadResponse("TestChoiceServiceResponse", producerNamespace)>]
    abstract TestChoiceService: [<XRoadElement("request")>] request: Types.TestChoice -> ResultTypes.TestChoiceServiceResult
    
    [<XRoadOperation("WithChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithChoiceService", producerNamespace)>]
    [<XRoadResponse("WithChoiceServiceResponse", producerNamespace)>]
    abstract WithChoiceService: [<XRoadElement("request")>] request: Types.WithChoice -> ResultTypes.WithChoiceServiceResult
    
    [<XRoadOperation("WithBinaryContentService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithBinaryContentService", producerNamespace)>]
    [<XRoadResponse("WithBinaryContentServiceResponse", producerNamespace)>]
    abstract WithBinaryContentService: [<XRoadElement("request")>] request: Types.WithBinaryContent -> ResultTypes.WithBinaryContentServiceResult
    
    [<XRoadOperation("WithXopBinaryContentService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithXopBinaryContentService", producerNamespace)>]
    [<XRoadResponse("WithXopBinaryContentServiceResponse", producerNamespace)>]
    abstract WithXopBinaryContentService: [<XRoadElement("request")>] request: Types.WithXopBinaryContent -> ResultTypes.WithXopBinaryContentServiceResult

    [<XRoadOperation("HasOptionalElementsService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("HasOptionalElementsService", producerNamespace)>]
    [<XRoadResponse("HasOptionalElementsServiceResponse", producerNamespace)>]
    abstract HasOptionalElementsService: [<XRoadElement("request")>] request: Types.HasOptionalElements -> ResultTypes.HasOptionalElementsServiceResult
    
    [<XRoadOperation("ArrayService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ArrayService", producerNamespace)>]
    [<XRoadResponse("ArrayServiceResponse", producerNamespace)>]
    abstract ArrayService: [<XRoadElement("request")>] request: string[] -> ResultTypes.Service2Result
    
    [<XRoadOperation("OptionalIntService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("OptionalIntService", producerNamespace)>]
    [<XRoadResponse("OptionalIntServiceResponse", producerNamespace)>]
    abstract OptionalIntService: [<XRoadElement("request")>] request: Optional.Option<int> -> ResultTypes.OptionalIntServiceResult
    
    [<XRoadOperation("Level3Service", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Level3Service", producerNamespace)>]
    [<XRoadResponse("Level3ServiceResponse", producerNamespace)>]
    abstract Level3Service: [<XRoadElement("request")>] request: Types.Level3 -> ResultTypes.Level3ServiceResult
    
    [<XRoadOperation("AbstractBaseService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("AbstractBaseService", producerNamespace)>]
    [<XRoadResponse("AbstractBaseServiceResponse", producerNamespace)>]
    abstract AbstractBaseService: [<XRoadElement("request")>] request: Types.AbstractBase -> ResultTypes.AbstractBaseServiceResult
    
    [<XRoadOperation("AbstractBaseWithNoSubTypesService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("AbstractBaseWithNoSubTypesService", producerNamespace)>]
    [<XRoadResponse("AbstractBaseWithNoSubTypesServiceResponse", producerNamespace)>]
    abstract AbstractBaseWithNoSubTypesService: [<XRoadElement("request")>] request: Types.AbstractBaseWithNoSubTypes -> ResultTypes.AbstractBaseWithNoSubTypesServiceResult

let deserialize (nm: string) (xml: string) context =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use textReader = new StringReader(xml)
    use reader = XmlReader.Create(textReader)
    while reader.Read() && reader.NodeType <> XmlNodeType.Element do ()
    map.Deserializer.Invoke(reader, context)

let serialize nm value context =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use stream = new MemoryStream()
    use sw = new StreamWriter(stream, Encoding.UTF8)
    use writer = XmlWriter.Create(sw)
    writer.WriteStartDocument()
    writer.WriteStartElement("Body")
    writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
    writer.WriteAttributeString("xmlns", "tns", XmlNamespace.Xmlns, producerNamespace)
    writer.WriteAttributeString("xmlns", "test", XmlNamespace.Xmlns, "testns")
    map.Serializer.Invoke(writer, value, context)
    writer.WriteEndElement()
    writer.WriteEndDocument()
    writer.Flush()
    stream.Position <- 0L
    use reader = new StreamReader(stream, Encoding.UTF8)
    reader.ReadToEnd()

let deserialize' nm value =
    deserialize nm value (SerializerContext())

let serialize' nm value =
    serialize nm value (SerializerContext())

let simpleTypeEntity =
    let entity = Types.SimpleType(Value = 13)
    entity.ComplexValue <- Types.ComplexType(String = "test", BigInteger = 100I)
    entity.SubContent <- Types.WithContent(ContentValue = true)
    entity
    
let getResponse<'T> name xml =
    let response = xml |> deserialize' name
    Expect.isTrue (response :? 'T) "wrong result type"
    response |> unbox<'T>

let [<Tests>] tests =
    testList "serialization tests" [
        test "can handle array type response" {
            let result: ResultTypes.Service2Result = getResponse "Service2" @"<Service2Response><response><item /><item /></response></Service2Response>"
            Expect.isNotNull result.response "response should have value"
            Expect.equal result.response.Length 2 "response should have exactly 2 items"
            Expect.equal result.response.[0] "" "should be empty string"
            Expect.equal result.response.[1] "" "should be empty string"
        }
        
        test "can serialize unit request" {
            let xml = serialize' "Service1" [||]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:Service1 /></Body>" "invalid xml result"
        }
        
        test "can deserialize unit response" {
            getResponse<ResultTypes.Service1Result> "Service1" @"<Service1 xmlns=""http://producer.x-road.eu/"" />"
            |> ignore
        }
        
        test "can serialize simple value" {
            let xml = serialize' "SimpleValueService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:SimpleValueService><request><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></request></tns:SimpleValueService></Body>" "invalid xml result"
        }
        
        test "can deserialize simple value" {
            let result: ResultTypes.SimpleValueServiceResult = getResponse "SimpleValueService" @"<SimpleValueServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns=""""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></response></SimpleValueServiceResponse>"
            Expect.equal result.response.Value 13 "wrong result.Value value"
            Expect.equal result.response.ComplexValue.BigInteger 100I "wrong result.ComplexValue.BigInteger value"
            Expect.equal result.response.ComplexValue.String "test" "wrong result.ComplexValue.String value"
            Expect.isTrue result.response.SubContent.ContentValue "wrong result.SubContent.ContentValue value"
        }
        
        test "serialize null value" {
            let xml = serialize' "StringService" [| (null: string) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:StringService><request xsi:nil=""true"" /></tns:StringService></Body>" "invalid serialization result"
        }
        
        test "deserialize null value" {
            let result: ResultTypes.StringServiceResult = getResponse "StringService" @"<StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xsi:nil=""true"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns="""" /></StringServiceResponse>"
            Expect.isNull result.response "response should be null"
        }
        
        test "write qualified root name" {
            let xml = serialize' "QualifiedRootService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:QualifiedRootService><root xmlns=""urn:some-namespace""><Value xmlns="""">13</Value><ComplexValue xmlns=""""><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent xmlns="""">true</SubContent></root></tns:QualifiedRootService></Body>" "invalid xml result"
        }
        
        test "serializing unserializable type" {
            Expect.throwsC
                (fun _ ->
                    let value = Types.UnserializableType(Value = 10)
                    serialize' "UnserializableService" [| value |] |> ignore)
                (fun e -> Expect.equal e.Message "Type `XRoadProvider.Tests.SerializationTest+Types+UnserializableType` is not serializable." "invalid exception")
        }
        
        test "serialize string value" {
            let xml = serialize' "StringService" [| "string value" |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:StringService><request>string value</request></tns:StringService></Body>" "invalid serialization result"
        }
        
        test "deserialize string value" {
            let result: ResultTypes.StringServiceResult = getResponse "StringService" @"<StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns="""">string value</response></StringServiceResponse>"
            Expect.equal result.response "string value" "response not equal to 'string value'"
        }
        
        test "serialize int value" {
            let xml = serialize' "IntService" [| 32 |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:IntService><request>32</request></tns:IntService></Body>" "invalid serialization result"
        }
        
        test "deserialize int value" {
            let result: ResultTypes.IntServiceResult = getResponse "IntService" @"<IntServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><response>32</response></IntServiceResponse>"
            Expect.equal result.response 32 "wrong value"
        }
        
        test "serialize nullable values" {
            let xml = serialize' "NullableService" [| Types.WithNullableMembers(Value1 = Nullable(13)) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:NullableService><request><Value1>13</Value1><Value2 xsi:nil=""true"" /></request></tns:NullableService></Body>" "invalid serialization result"
        }
        
        test "deserialize nullable values" {
            let result: ResultTypes.NullableServiceResult = getResponse "NullableService" @"<tns:NullableService xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Value1>13</Value1><Value2 xsi:nil=""true"" /></response></tns:NullableService>"
            Expect.equal result.response.Value1 (Nullable 13) "wrong Value1 value"
            Expect.equal result.response.Value2 (Nullable()) "wrong Value2 value"
        }
        
        test "serialize not nullable as null" {
            Expect.throwsC
                (fun _ -> serialize' "ComplexTypeService" [| Types.ComplexType(String = null) |] |> ignore)
                (fun e -> Expect.equal e.Message "Not nullable property `String` of type `ComplexType` has null value." "invalid exception message")
        }
        
        test "serialize choice with abstract root element" {
            let optionEntity = Types.Concrete1(SubValue1 = "test2", BaseValue = "test")
            let xml = serialize' "AbstractChoiceService" [| Types.TypeWithAbstractChoice(X = Types.AbstractRootChoice.New_value1(optionEntity)) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:AbstractChoiceService><request><value1 xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></value1></request></tns:AbstractChoiceService></Body>" "invalid serialization result"
        }
        
        test "deserialize choice with abstract root element" {
            let result: ResultTypes.AbstractChoiceServiceResult = getResponse "AbstractChoiceService" @"<tns:AbstractChoiceServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><value1 xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></value1></response></tns:AbstractChoiceServiceResponse>"
            let (success, value) = result.response.X.TryGet_value1()
            Expect.isTrue success "response should contain value1"
            Expect.isNotNull value "response value should not be null"
            Expect.equal value.BaseValue "test" "invalid value"
        }
        
        test "serialize array with default property names" {
            let entity = Types.WithArray1(Array = [| true; false; true; true |])
            let xml = serialize' "WithArray1Service" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithArray1Service><request><Array><item>true</item><item>false</item><item>true</item><item>true</item></Array></request></tns:WithArray1Service></Body>" "invalid serialization result"
        }
        
        test "deserialize array with default property names" {
            let result: ResultTypes.WithArray1ServiceResult = getResponse "WithArray1Service" @"<tns:WithArray1ServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Array><item>true</item><item>false</item><item>true</item><item>true</item></Array></response></tns:WithArray1ServiceResponse>"
            Expect.isNotNull result.response.Array ""
            Expect.equal result.response.Array [| true; false; true; true |] ""
        }
        
        test "deserialize abstract type" {
            Expect.throwsC
                (fun _ -> getResponse<ResultTypes.AbstractBaseServiceResult> "AbstractBaseService" @"<AbstractBaseServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><response><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></response></AbstractBaseServiceResponse>" |> ignore)
                (fun e -> Expect.equal e.Message "Cannot deserialize abstract type `AbstractBase`." "")
        }
        
        test "deserialize abstract type with no sub types" {
            Expect.throwsC
                (fun _ -> getResponse<ResultTypes.AbstractBaseWithNoSubTypesServiceResult> "AbstractBaseWithNoSubTypesService" @"<AbstractBaseWithNoSubTypesServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><response><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></response></AbstractBaseWithNoSubTypesServiceResponse>" |> ignore)
                (fun e -> Expect.equal e.Message "Cannot deserialize abstract type `AbstractBaseWithNoSubTypes`." "")
        }
        
        test "serialize extended type with base type contents" {
            let entity = Types.ExtendedType(OwnElement = "test", String = "test", BigInteger = 100I)
            let xml = serialize' "ExtendedTypeService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ExtendedTypeService><request xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></request></tns:ExtendedTypeService></Body>" "invalid serialization result"
        }
        
        test "deserialize extended type with base type contents" {
            let result: ResultTypes.ComplexTypeServiceResult = getResponse "ExtendedTypeService" @"<tns:ExtendedTypeServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></response></tns:ExtendedTypeServiceResponse>"
            Expect.equal result.response.BigInteger 100I ""
            Expect.equal result.response.String "test" ""
            Expect.isTrue (result.response :? Types.ExtendedType) ""
            let extendedType = result.response |> unbox<Types.ExtendedType>
            Expect.equal extendedType.OwnElement "test" ""
        }
        
        test "serialize base type when subtype is used" {
            let entityMember = Types.ExtendedType(OwnElement = "test", String = "test", BigInteger = 100I)
            let entity = Types.UseBaseClass(Member = entityMember)
            let xml = serialize' "UseBaseClassService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:UseBaseClassService><request><Member xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></request></tns:UseBaseClassService></Body>" "invalid serialization result"
        }
        
        test "deserialize base type when subtype is used" {
            let result: ResultTypes.UseBaseClassServiceResult = getResponse "UseBaseClassService" @"<tns:UseBaseClassServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Member xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></response></tns:UseBaseClassServiceResponse>"
            Expect.isTrue (result.response.Member :? Types.ExtendedType) ""
            Expect.equal result.response.Member.BigInteger 100I ""
            let extendedType = result.response.Member |> unbox<Types.ExtendedType>
            Expect.equal extendedType.OwnElement "test" ""
            Expect.equal result.response.Member.String "test" ""
        }
        
        test "serialize abstract base type when subtype is used" {
            let concreteEntity = Types.Concrete1(SubValue1 = "test2", BaseValue = "test")
            let entity = Types.Referrer(Reference = concreteEntity)
            let xml = serialize' "ReferrerService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ReferrerService><request><Reference xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></request></tns:ReferrerService></Body>" "invalid serialization result"
        }
        
        test "deserialize abstract base type when subtype is used" {
            let result: ResultTypes.ReferrerServiceResult = getResponse "ReferrerService" @"<tns:ReferrerServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Reference xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></response></tns:ReferrerServiceResponse>"
            Expect.isNotNull result.response.Reference ""
            Expect.isTrue (result.response.Reference  :? Types.Concrete1) ""
            let concrete1 = result.response.Reference |> unbox<Types.Concrete1>
            Expect.equal concrete1.SubValue1 "test2" ""
        }
        
        test "serialize abstract base type when subtype is used (with explicit name and namespace)" {
            let concreteEntity = Types.Concrete3(SubValue3 = "test2", BaseValue = "test")
            let entity = Types.Referrer(Reference = concreteEntity)
            let xml = serialize' "ReferrerService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ReferrerService><request><Reference xsi:type=""test:ConcreteTypeName""><BaseValue>test</BaseValue><SubValue3>test2</SubValue3></Reference></request></tns:ReferrerService></Body>" "invalid serialization result"
        }
        
        test "deserialize abstract base type when subtype is used (with explicit name and namespace)" {
            let result: ResultTypes.ReferrerServiceResult = getResponse "ReferrerService" @"<tns:ReferrerServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Reference xsi:type=""test:ConcreteTypeName""><BaseValue>test</BaseValue><SubValue3>test2</SubValue3></Reference></response></tns:ReferrerServiceResponse>"
            Expect.isNotNull result.response.Reference ""
            Expect.isTrue (result.response.Reference :? Types.Concrete3) ""
            let concrete3 = result.response.Reference |> unbox<Types.Concrete3>
            Expect.equal concrete3.SubValue3 "test2" ""
        }
        
        test "serialize inner abstract base type" {
            let referenceEntity = Types.Concrete1(SubValue1 = "kino", BaseValue = "basev")
            let referrerEntity = Types.Referrer(Reference = referenceEntity)
            let entity = Types.InnerReferrer(Ref = referrerEntity)
            let xml = serialize' "InnerReferrerService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:InnerReferrerService><request><Ref><Reference xsi:type=""Concrete1""><BaseValue>basev</BaseValue><SubValue1>kino</SubValue1></Reference></Ref></request></tns:InnerReferrerService></Body>" "invalid serialization result"
        }
        
        test "deserialize inner abstract base type" {
            let result: ResultTypes.InnerReferrerServiceResult = getResponse "InnerReferrerService" @"<tns:InnerReferrerServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Ref><Reference xsi:type=""Concrete1""><BaseValue>basev</BaseValue><SubValue1>kino</SubValue1></Reference></Ref></response></tns:InnerReferrerServiceResponse>"
            Expect.isNotNull result.response.Ref.Reference ""
            Expect.isTrue (result.response.Ref.Reference :? Types.Concrete1) ""
            let concrete1 = result.response.Ref.Reference |> unbox<Types.Concrete1>
            Expect.equal concrete1.SubValue1 "kino" ""
            Expect.equal result.response.Ref.Reference.BaseValue "basev" ""
        }
        
        test "serialize choice type 1" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let entity = Types.TestChoice.NewChoice1(optionEntity)
            let xml = serialize' "TestChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:TestChoiceService><Choice1Element>test</Choice1Element></tns:TestChoiceService></Body>" "invalid serialization result"
        }
        
        test "deserialize choice type 1" {
            let result: ResultTypes.TestChoiceServiceResult = getResponse "TestChoiceService" @"<tns:TestChoiceServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><Choice1Element>test</Choice1Element></tns:TestChoiceServiceResponse>"
            let (success, value) = result.response.TryGetChoice1()
            Expect.isTrue success "result should be choice 1"
            Expect.equal value.Choice1Element "test" "wrong choice 1 element value"
            let (success, value) = result.response.TryGetChoice2()
            Expect.isFalse success "result should not be choice 2"
        }
        
        test "serialize choice type 2" {
            let optionEntity = Types.Choice2(Choice2Element = "test")
            let entity = Types.TestChoice.NewChoice2(optionEntity)
            let xml = serialize' "TestChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:TestChoiceService><Choice2><Choice2Element>test</Choice2Element></Choice2></tns:TestChoiceService></Body>" "invalid serialization result"
        }
        
        test "deserialize choice type 2" {
            let result: ResultTypes.TestChoiceServiceResult = getResponse "TestChoiceService" @"<tns:TestChoiceServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><Choice2><Choice2Element>test</Choice2Element></Choice2></tns:TestChoiceServiceResponse>"
            let (success, value) = result.response.TryGetChoice1()
            Expect.isFalse success "should not be choice 1"
            let (success, value) = result.response.TryGetChoice2()
            Expect.isTrue success "should be choice 2"
            Expect.equal value.Choice2Element "test" "wrong choice 2 element content"
        }
        
        test "serialize inner choice 1 element" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice1(optionEntity)
            let entity = Types.WithChoice(NotAChoice = "tere", IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice>tere</NotAChoice><Choice1Element>test</Choice1Element></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        test "deserialize inner choice 1 element" {
            let result: ResultTypes.WithChoiceServiceResult = getResponse "WithChoiceService" @"<tns:WithChoiceServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><NotAChoice>tere</NotAChoice><Choice1Element>test</Choice1Element></response></tns:WithChoiceServiceResponse>"
            Expect.equal result.response.NotAChoice "tere" "wrong not a choice value"
            let (success, value) = result.response.IsAChoice.TryGetChoice1()
            Expect.isTrue success "should be choice 1"
            Expect.equal value.Choice1Element "test" "invalid choice 1 element content"
            let (success, value) = result.response.IsAChoice.TryGetChoice2()
            Expect.isFalse success "should not be choice 2"
        }
        
        test "serialize inner choice 2 element" {
            let optionEntity = Types.Choice2(Choice2Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice2(optionEntity)
            let entity = Types.WithChoice(NotAChoice = "tere", IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice>tere</NotAChoice><Choice2><Choice2Element>test</Choice2Element></Choice2></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        test "deserialize inner choice 2 element" {
            let result: ResultTypes.WithChoiceServiceResult = getResponse "WithChoiceService" @"<tns:WithChoiceServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><NotAChoice>tere</NotAChoice><Choice2><Choice2Element>test</Choice2Element></Choice2></response></tns:WithChoiceServiceResponse>"
            Expect.equal result.response.NotAChoice "tere" "wrong not a choice value"
            let (success, value) = result.response.IsAChoice.TryGetChoice1()
            Expect.isFalse success "should not be choice1"
            let (success, value) = result.response.IsAChoice.TryGetChoice2()
            Expect.isTrue success "should be choice2"
            Expect.equal value.Choice2Element "test" "wrong choice 2 element value"
        }
        
        test "serialize empty string" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice1(optionEntity)
            let entity = Types.WithChoice(NotAChoice = "", IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice /><Choice1Element>test</Choice1Element></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        test "deserialize empty string" {
            let result: ResultTypes.WithChoiceServiceResult = getResponse "WithChoiceService" @"<tns:WithChoiceServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><NotAChoice /><Choice1Element>test</Choice1Element></response></tns:WithChoiceServiceResponse>"
            Expect.equal result.response.NotAChoice "" "not a choice should be empty string"
            let (success, value) = result.response.IsAChoice.TryGetChoice1()
            Expect.isTrue success "is a choice should contain choice 1"
            Expect.equal "test" value.Choice1Element "wrong choice 1 value"
        }
        
        test "serialize null string" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice1(optionEntity)
            let entity = Types.WithChoice(NotAChoice = null, IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice xsi:nil=""true"" /><Choice1Element>test</Choice1Element></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        test "deserialize null string" {
            let result: ResultTypes.WithChoiceServiceResult = getResponse "WithChoiceService" @"<tns:WithChoiceServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><NotAChoice xsi:nil=""true"" /><Choice1Element>test</Choice1Element></response></tns:WithChoiceServiceResponse>"
            Expect.isNull result.response.NotAChoice "not a choice should be null"
            let (success, value) = result.response.IsAChoice.TryGetChoice1()
            Expect.isTrue success "is a choice should contain choice 1"
            Expect.equal "test" value.Choice1Element "wrong choice value content"
        }
        
        test "serialize null array" {
            let xml = serialize' "WithArray1Service" [| Types.WithArray1() |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithArray1Service><request><Array xsi:nil=""true"" /></request></tns:WithArray1Service></Body>" "invalid serialization result"
        }
        
        test "deserialize null array" {
            let result: ResultTypes.WithArray1ServiceResult = getResponse "WithArray1Service" @"<tns:WithArray1ServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Array xsi:nil=""true"" /></response></tns:WithArray1ServiceResponse>"
            Expect.isNull result.response.Array "result array should be null"
        }
        
        test "serialize inline file" {
            let context = SerializerContext()
            let entity = Types.WithBinaryContent(BinaryContent=BinaryContent.Create([| 1uy; 2uy; 3uy; 4uy |]))
            let xml = serialize "WithBinaryContentService" [| entity |] context
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithBinaryContentService><request><BinaryContent>AQIDBA==</BinaryContent></request></tns:WithBinaryContentService></Body>" "invalid serialization result"
            Expect.isEmpty context.Attachments "no serialized attachments was expected"
        }
        
        test "deserialize inline file" {
            let result: ResultTypes.WithBinaryContentServiceResult = getResponse "WithBinaryContentService" @"<tns:WithBinaryContentServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><BinaryContent>AQIDBA==</BinaryContent></response></tns:WithBinaryContentServiceResponse>"
            Expect.isNotNull result.response.BinaryContent "binary content was not deserialized"
            Expect.isNotNull result.response.BinaryContent.ContentID "content id should have value"
            Expect.equal (result.response.BinaryContent.GetBytes()) [| 1uy; 2uy; 3uy; 4uy |] "wrong content"
        }
        
        test "serialize multipart file" {
            let context = SerializerContext(IsMultipart=true)
            let entity = Types.WithBinaryContent(BinaryContent=BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let xml = serialize "WithBinaryContentService" [| entity |] context
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithBinaryContentService><request><BinaryContent href=""cid:Content-ID"" /></request></tns:WithBinaryContentService></Body>" "invalid serialization result"
            Expect.equal 1 context.Attachments.Count "result should have exactly 1 attachment"
            Expect.isTrue (context.Attachments.ContainsKey("Content-ID")) "attachment has wrong key"
        }
        
        test "deserialize multipart file" {
            let context = SerializerContext()
            context.Attachments.Add("Content-ID", BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let xml = @"<tns:WithBinaryContentServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><BinaryContent href=""cid:Content-ID"" /></response></tns:WithBinaryContentServiceResponse>"
            let response = deserialize "WithBinaryContentService" xml context
            Expect.isTrue (response :? ResultTypes.WithBinaryContentServiceResult) "wrong result type"
            let result = response |> unbox<ResultTypes.WithBinaryContentServiceResult>
            Expect.isNotNull result.response.BinaryContent "binary content was not deserialized"
            Expect.equal result.response.BinaryContent.ContentID "Content-ID" "wrong content id"
            Expect.isTrue (result.response.BinaryContent = context.Attachments.["Content-ID"]) "should be same content"
            Expect.equal (result.response.BinaryContent.GetBytes()) [| 1uy; 2uy; 3uy; 4uy |] "wrong content"
        }
        
        test "serialize xop file" {
            let context = SerializerContext()
            let entity = Types.WithXopBinaryContent(BinaryContent=BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let xml = serialize "WithXopBinaryContentService" [| entity |] context
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithXopBinaryContentService><request><BinaryContent><xop:Include href=""cid:Content-ID"" xmlns:xop=""http://www.w3.org/2004/08/xop/include"" /></BinaryContent></request></tns:WithXopBinaryContentService></Body>" "invalid serialization result"
            Expect.equal 1 context.Attachments.Count "result should have exactly 1 attachment"
            Expect.isTrue (context.Attachments.ContainsKey("Content-ID")) "attachment has wrong key"
        }
        
        test "deserialize xop file" {
            let context = SerializerContext()
            context.Attachments.Add("Content-ID", BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let xml = @"<tns:WithXopBinaryContentServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><BinaryContent><xop:Include href=""cid:Content-ID"" xmlns:xop=""http://www.w3.org/2004/08/xop/include"" /></BinaryContent></response></tns:WithXopBinaryContentServiceResponse>"
            let response = deserialize "WithXopBinaryContentService" xml context
            Expect.isTrue (response :? ResultTypes.WithXopBinaryContentServiceResult) "wrong result type"
            let result = response |> unbox<ResultTypes.WithXopBinaryContentServiceResult>
            Expect.isNotNull result.response.BinaryContent "binary content was not deserialized"
            Expect.equal result.response.BinaryContent.ContentID "Content-ID" "wrong content id"
            Expect.isTrue (result.response.BinaryContent = context.Attachments.["Content-ID"]) "should be same content"
            Expect.equal (result.response.BinaryContent.GetBytes()) [| 1uy; 2uy; 3uy; 4uy |] "wrong content" 
        }
        
        test "can serialize type with optional reference type members" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements(Value1 = Optional.Option.Some<string>("value")) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request><Value1>value</Value1></request></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        test "can deserialize type with optional reference type members" {
            let result: ResultTypes.HasOptionalElementsServiceResult = getResponse "HasOptionalElementsService" @"<tns:HasOptionalElementsServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Value1>value</Value1></response></tns:HasOptionalElementsServiceResponse>"
            Expect.isFalse result.response.Array1.HasValue "array should be none"
            Expect.isTrue result.response.Value1.HasValue "value1 should be some"
            Expect.equal (result.response.Value1.ValueOr "") "value" "value1 has wrong content"
            Expect.isFalse result.response.Value2.HasValue "value2 should be none"
        }
        
        test "can serialize type with optional value type members" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements(Value2 = Optional.Option.Some<int32>(15)) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request><Value2>15</Value2></request></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        test "can deserialize type with optional value type members" {
            let result: ResultTypes.HasOptionalElementsServiceResult = getResponse "HasOptionalElementsService" @"<tns:HasOptionalElementsServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Value2>15</Value2></response></tns:HasOptionalElementsServiceResponse>"
            Expect.isFalse result.response.Array1.HasValue "array should be none"
            Expect.isFalse result.response.Value1.HasValue "value1 should be none"
            Expect.isTrue result.response.Value2.HasValue "value2 should be some"
            Expect.equal (result.response.Value2.ValueOr 0) 15 "value2 has wrong content"
        }
        
        test "can serialize type with optional array type members" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements(Array1 = Optional.Option.Some<int[]>([| 1; 2; 3 |])) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request><Array1><item>1</item><item>2</item><item>3</item></Array1></request></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        test "can deserialize type with optional array type members" {
            let result: ResultTypes.HasOptionalElementsServiceResult = getResponse "HasOptionalElementsService" @"<tns:HasOptionalElementsServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Array1><item>1</item><item>2</item><item>3</item></Array1></response></tns:HasOptionalElementsServiceResponse>"
            Expect.isTrue result.response.Array1.HasValue "array should be some"
            Expect.equal (result.response.Array1.ValueOr [||]) [| 1; 2; 3 |] "array has wrong content"
            Expect.isFalse result.response.Value1.HasValue "value1 should be none"
            Expect.isFalse result.response.Value2.HasValue "value2 should be none"
        }
        
        test "can serialize type with no optional members set" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements() |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request /></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        test "can deserialize type with no optional members set" {
            let result: ResultTypes.HasOptionalElementsServiceResult = getResponse "HasOptionalElementsService" @"<tns:HasOptionalElementsServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response /></tns:HasOptionalElementsServiceResponse>"
            Expect.isFalse result.response.Array1.HasValue "array should be none"
            Expect.isFalse result.response.Value1.HasValue "value1 should be none"
            Expect.isFalse result.response.Value2.HasValue "value2 should be none"
        }
        
        test "serialize array of system type values" {
            let xml = serialize' "ArrayService" [| [| "1"; "2"; "3" |] |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ArrayService><request><item>1</item><item>2</item><item>3</item></request></tns:ArrayService></Body>" "invalid serialization result"
        }
        
        test "deserialize array of system type values" {
            let result: ResultTypes.Service2Result = getResponse "ArrayService"  @"<tns:ArrayServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><item>1</item><item>2</item><item>3</item></response></tns:ArrayServiceResponse>"
            Expect.equal result.response [| "1"; "2"; "3" |] "wrong result content"
        }
        
        test "serialize root optional some value" {
            let initial = Optional.Option.Some<int>(202)
            let xml = serialize' "OptionalIntService" [| initial |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:OptionalIntService><request>202</request></tns:OptionalIntService></Body>" "invalid serialization result"
        }
        
        test "deserialize root optional some value" {
            let result: ResultTypes.OptionalIntServiceResult = getResponse "OptionalIntService" @"<tns:OptionalIntServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response>202</response></tns:OptionalIntServiceResponse>"
            Expect.equal result.response (Optional.Option.Some<int>(202)) "wrong result value"
        }
        
        test "serialize root optional none value" {
            let initial = Optional.Option.None<int>()
            let xml = serialize' "OptionalIntService" [| initial |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:OptionalIntService /></Body>" "invalid serialization result"
        }
        
        test "deserialize root optional none value" {
            let result: ResultTypes.OptionalIntServiceResult = getResponse "OptionalIntService" @"<tns:OptionalIntServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns"" />"
            Expect.isFalse result.response.HasValue "result should be None"
        }
        
        test "serialize multiple levels of inheritance" {
            let initial = Types.Level3(Value1 = Nullable<int>(1), Value2 = Nullable<int>(2), Value3 = Nullable<int>(3))
            let xml = serialize' "Level3Service" [| initial |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:Level3Service><request><Value1>1</Value1><Value2>2</Value2><Value3>3</Value3></request></tns:Level3Service></Body>" "invalid serialization result"
        }
        
        test "deserialize multiple levels of inheritance" {
            let result: ResultTypes.Level3ServiceResult = getResponse "Level3Service" @"<tns:Level3ServiceResponse xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><response><Value1>1</Value1><Value2>2</Value2><Value3>3</Value3></response></tns:Level3ServiceResponse>"
            Expect.equal result.response.Value1 (Nullable 1) ""
            Expect.equal result.response.Value2 (Nullable 2) ""
            Expect.equal result.response.Value3 (Nullable 3) ""
        }
    ]
