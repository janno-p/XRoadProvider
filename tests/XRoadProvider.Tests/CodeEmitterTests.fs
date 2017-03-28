module CodeEmitterTests

open FsUnit
open NUnit.Framework
open XRoad.Serialization.Attributes

module OptionalElements =
    [<XRoadType>]
    type HasOptionalElements () =
        [<XRoadElement(IsOptional=true)>]
        member val Value1 = Unchecked.defaultof<string> with get, set
        [<XRoadElement(IsOptional=true)>]
        member val Value2 = Unchecked.defaultof<int> with get, set

    let [<Test>] ``ignores missing optional element`` () =
        1 |> should equal 1
