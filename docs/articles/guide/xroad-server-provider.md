# XRoadServer6 Type Provider #



## Type provider for producer discovery ##

XRoadProvider package includes separate type provider to retrieve producer information from security
server. Resulting type contains details about all producers available and their WSDL URIs which may
be used as parameter to `XRoadProducer` provider to initialize service interfaces.

Example use of `XRoadServer` type provider:

```fsharp
open XRoadProvider

// Acquire list of producer from security server.
let [<Literal>] securityServerUrl = "http://your-security-server-here/"

type SecurityServer = XRoadServer6<securityServerUrl, "ee-dev", "COM", "12345678", "generic-consumer">
type AdsConfig = SecurityServer.Producers.GOV.``Maa-amet (70003098)``.``SUBSYSTEM:ads``
type Ads = XRoadProducer<AdsConfig.``SERVICE:ADSaadrmuudatused``>

let adsService = Ads.xroadeuService.xroadeuServicePort(securityServerUrl)

let adsHeader =
    XRoadHeader(
        Client = SecurityServer.Identifier,
        Producer = AdsConfig.Identifier,
        ProtocolVersion = "4.0"
    )

let adsResponse =
    adsService.ADSaadrmuudatused(
        adsHeader,
        muudetudAlates = some(NodaTime.LocalDate(2017, 11, 28))
    )

adsResponse.fault.MatchSome(fun f -> failwithf "Invalid service response. %s: %s" f.faultCode f.faultString)

let muudatused = adsResponse.muudatused.ValueOr([||])
printfn "Got %d changes in response message." muudatused.Length
```
