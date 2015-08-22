module XRoadSerializer.Tests.Serialization

open FsUnit
open NUnit.Framework
open XRoad

let [<Test>] ``initializes new serializer`` () =
    let serializer = Serializer()
    serializer |> should not' (be null)
