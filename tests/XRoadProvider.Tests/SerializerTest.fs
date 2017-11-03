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
