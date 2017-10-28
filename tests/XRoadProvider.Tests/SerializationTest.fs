module XRoadProvider.Tests.SerializationTest

open Expecto
open XRoad

let [<Literal>] producerName = "producer"
let [<Literal>] producerUri = "http://producer.x-road.eu/"

type Services =
    abstract Service1: unit -> unit

let [<Tests>] tests =
    testList "serialization tests" [
        test "" {
            Expect.throwsT<exn>
                (fun _ -> XRoadUtil.MakeServiceCall(typeof<Services>, "Service1", producerUri, producerName, null, [||]) |> ignore)
                "it is not implemented yet!"
        }
    ]
