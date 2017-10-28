module XRoadProvider.Tests.Program

open Expecto
open Suave
open System.Threading

[<EntryPoint>]
let main args =
    let cts = new CancellationTokenSource()
    let config = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync defaultConfig (Successful.OK "Hello, World!")
    
    Async.Start(server, cts.Token)

    let result = runTestsInAssembly Expecto.Tests.defaultConfig args
    
    cts.Cancel()
    
    result
