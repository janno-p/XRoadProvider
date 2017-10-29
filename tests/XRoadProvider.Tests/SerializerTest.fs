module XRoadSerializer.Tests.SerializerTest

open Expecto
open System
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Xml
open XRoad
open XRoad.DynamicMethods
open XRoad.Serialization.Attributes

module TestType =
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

    [<XRoadType(LayoutKind.Sequence)>]
    type ExtendedType() =
        inherit ComplexType()
        [<XRoadElement>]
        member val OwnElement = Unchecked.defaultof<string> with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type UseBaseClass() =
        [<XRoadElement>]
        member val Member = Unchecked.defaultof<ComplexType> with get, set

    [<AbstractClass; AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type AbstractBaseWithNoSubTypes() =
        [<XRoadElement>]
        member val BaseValue = Unchecked.defaultof<string> with get, set

    [<AbstractClass; AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type AbstractBase() =
        [<XRoadElement>]
        member val BaseValue = Unchecked.defaultof<string> with get, set

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

    [<XRoadType(LayoutKind.Sequence)>]
    type Referrer() =
        [<XRoadElement>]
        member val Reference = Unchecked.defaultof<AbstractBase> with get, set

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
    type InnerReferrer() =
        [<XRoadElement>]
        member val Ref = Unchecked.defaultof<Referrer> with get, set

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

type Services =
    class
    end

let serializeWithContext nm (nslist: (string * string) list) (context: SerializerContext) value =
    use stream = new MemoryStream()
    use sw = new StreamWriter(stream, Encoding.UTF8)
    use writer = XmlWriter.Create(sw)
    writer.WriteStartDocument()
    writer.WriteStartElement("wrapper")
    writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
    nslist |> List.iter (fun (pr,ns) -> writer.WriteAttributeString("xmlns", pr, XmlNamespace.Xmlns, ns))
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    map.Serialize(writer, context, value)
    writer.WriteEndElement()
    writer.WriteEndDocument()
    writer.Flush()
    sw.Flush()
    stream.Position <- 0L
    use sr = new StreamReader(stream, Encoding.UTF8)
    sr.ReadToEnd()

let serialize' nm v = serializeWithContext nm [] (SerializerContext()) v
let serializeWithContext' nm context v = serializeWithContext nm [] context v

let deserializeWithContext<'T> nm (context: SerializerContext) xml : 'T =
    use sr = new StringReader(xml)
    use reader = XmlReader.Create(sr)
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    map.Deserialize(reader, context) |> unbox

let deserialize'<'T> nm xml : 'T = deserializeWithContext nm (SerializerContext()) xml
let deserializeWithContext'<'T> nm context xml : 'T = deserializeWithContext nm context xml

let simpleTypeEntity =
    let entity = TestType.SimpleType(Value = 13)
    entity.ComplexValue <- TestType.ComplexType(String = "test", BigInteger = 100I)
    entity.SubContent <- TestType.WithContent(ContentValue = true)
    entity

let [<Tests>] tests =
    ptestList "serializer tests" [
        test "can serialize simple value" {
            failtest "needs review"
            (*
            let resultXml = simpleTypeEntity |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.SimpleType>
            result |> should not' (be Null)
            result.Value |> should equal 13
            result.ComplexValue |> should not' (be Null)
            result.ComplexValue.BigInteger |> should equal 100I
            result.ComplexValue.String |> should equal "test"
            result.SubContent |> should not' (be Null)
            result.SubContent.ContentValue |> should equal true
            *)
        }
        
        test "serialize null value" {
            failtest "needs review"
            (*
            let resultXml = (null: string) |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha xsi:nil=""true"" /></wrapper>"
            resultXml |> deserialize'<string> |> should be Null
            *)
        }
        
        test "write qualified root name" {
            failtest "needs review"
            (*
            let resultXml = simpleTypeEntity |> serializeWithContext (XmlQualifiedName("root", "urn:some-namespace")) [] (SerializerContext())
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><root xmlns=""urn:some-namespace""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></root></wrapper>"
            *)
        }
        
        test "serializing unserializable type" {
            failtest "needs review"
            (*
            (fun () -> TestType.UnserializableType(Value = 10) |> serialize' |> ignore)
            |> should (throwWithMessage "Type `XRoadSerializer.Tests.SerializerTest+TestType+UnserializableType` is not serializable.") typeof<Exception>
            *)
        }
        
        test "serialize string value" {
            failtest "needs review"
            (*
            let resultXml = "string value" |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>string value</keha></wrapper>"
            resultXml |> deserialize'<string> |> should equal "string value"
            *)
        }
        
        test "serialize integer value" {
            failtest "needs review"
            (*
            let resultXml = 32 |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>32</keha></wrapper>"
            resultXml |> deserialize'<int> |> should equal 32
            *)
        }
        
        test "serialize nullable values" {
            failtest "needs review"
            (*
            let resultXml = TestType.WithNullableMembers(Value1 = Nullable(13)) |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value1>13</Value1><Value2 xsi:nil=""true"" /></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithNullableMembers>
            result |> should not' (be Null)
            result.Value1 |> should not' (be Null)
            result.Value1 |> should equal 13
            result.Value2 |> should be Null
            *)
        }
        
        test "serialize not nullable as null" {
            failtest "needs review"
            (*
            (fun () -> TestType.ComplexType(String = null) |> serialize' |> ignore)
            |> should (throwWithMessage "Not nullable property `String` of type `ComplexType` has null value.") typeof<Exception>
            *)
        }
        
        test "serialize choice with abstract root element" {
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
        
        test "serialize array with default property names" {
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
        
        test "deserialize abstract type" {
            failtest "needs review"
            (*
            (fun () ->
                @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></keha></wrapper>"
                |> deserialize'<TestType.AbstractBase>
                |> ignore)
            |> should (throwWithMessage "Cannot deserialize abstract type `AbstractBase`.") typeof<Exception>
            *)
        }
        
        test "deserialize abstract type with no sub types" {
            failtest "needs review"
            (*
            (fun () ->
                @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></keha></wrapper>"
                |> deserialize'<TestType.AbstractBaseWithNoSubTypes>
                |> ignore)
            |> should (throwWithMessage "Cannot deserialize abstract type `AbstractBaseWithNoSubTypes`.") typeof<Exception>
            *)
        }
        
        test "serialize extended type with base type contents" {
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
        
        test "serialize base type when subtype is used" {
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
        
        test "serialize abstract base type when subtype is used" {
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
        
        test "serialize abstract base type when subtype is used (with explicit name and namespace)" {
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
        
        test "serialize inner abstract base type" {
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
        
        test "serialize choice type 1" {
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
        
        test "serialize choice type 2" {
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
        
        test "serialize inner choice 1 element" {
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
        
        test "serialize inner choice 2 element" {
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
        
        test "serialize empty string" {
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
        
        test "deserialize null string" {
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
        
        test "serialize null array" {
            failtest "needs review"
            (*
            let resultXml = TestType.WithArray1() |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Array xsi:nil=""true"" /></keha></wrapper>"
            let result = resultXml |> deserialize'<TestType.WithArray1>
            result |> should not' (be Null)
            result.Array |> should be Null
            *)
        }
        
        test "serialize inline file" {
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
        
        test "serialize multipart file" {
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
        
        test "serialize xop file" {
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
        
        test "can serialize type with optional reference type members" {
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
        
        test "can serialize type with optional value type members" {
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
        
        test "can serialize type with optional array type members" {
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
        
        test "can serialize type with no optional members set" {
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
        
        test "serialize array of system type values" {
            failtest "needs review"
            (*
            let resultXml = [| "1"; "2"; "3" |] |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><item>1</item><item>2</item><item>3</item></keha></wrapper>"
            resultXml |> deserialize'<string> |> should equal [| "1"; "2"; "3" |]
            *)
        }
        
        test "serialize root optional some value" {
            failtest "needs review"
            (*
            let initial = Optional.Option.Some<int>(202)
            let resultXml = initial |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>202</keha></wrapper>"
            resultXml |> deserialize'<Optional.Option<int>> |> should equal initial
            *)
        }
        
        test "serialize root optional none value" {
            failtest "needs review"
            (*
            let initial = Optional.Option.None<int>()
            let resultXml = initial |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" />"
            resultXml |> deserialize'<Optional.Option<int>> |> should equal initial
            *)
        }
        
        test "serialize multiple levels of inheritance" {
            failtest "needs review"
            (*
            let initial = Level3(Value1 = Nullable<int>(1), Value2 = Nullable<int>(2), Value3 = Nullable<int>(3))
            let resultXml = initial |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value1>1</Value1><Value2>2</Value2><Value3>3</Value3></keha></wrapper>"
            resultXml |> deserialize'<Level3> |> should equal initial
            *)
        }
    ]
