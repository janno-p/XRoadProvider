module XRoadProvider.Tests.Program

open Expecto
open Suave
open Suave.RequestErrors
open System.Threading

[<EntryPoint>]
let main args =
    let app =
        choose
            [ XRoadProvider.Tests.SerializationTest.routes
              NOT_FOUND "Found no handlers" ]

    let cts = new CancellationTokenSource()
    let config = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync defaultConfig app
    
    Async.Start(server, cts.Token)

    let result = runTestsInAssembly Expecto.Tests.defaultConfig args
    
    cts.Cancel()
    
    result
