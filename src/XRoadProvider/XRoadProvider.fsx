#I __SOURCE_DIRECTORY__

#r "Common.Logging"
#r "Common.Logging.Core"
#r "NodaTime"
#r "Optional"
#r "System.Xml.Linq"
#r "XRoadProvider"

let (!?) = System.Nullable<'T>

let none<'T> = Optional.Option.None<'T>()
let some = Optional.Option.Some<'T>

let addRequestTrace () =
    let properties = Common.Logging.Configuration.NameValueCollection()
    properties.["showDateTime"] <- "true"
    properties.["level"] <- "TRACE"

    Common.Logging.LogManager.Adapter <- Common.Logging.Simple.ConsoleOutLoggerFactoryAdapter(properties)
